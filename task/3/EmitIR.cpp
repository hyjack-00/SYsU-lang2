#include "EmitIR.hpp"
#include "Obj.hpp"
#include "asg.hpp"

#include <llvm/IR/ValueSymbolTable.h>
#include <vector>

#define self (*this)

using namespace asg;

EmitIR::EmitIR(Obj::Mgr& mgr, llvm::LLVMContext& ctx, llvm::StringRef mid)
  : mMgr(mgr)
  , mMod(mid, ctx)
  , mCtx(ctx)
  , mIntTy(llvm::Type::getInt32Ty(ctx))
  , mCurIrb(std::make_unique<llvm::IRBuilder<>>(ctx))
  , mCtorTy(llvm::FunctionType::get(llvm::Type::getVoidTy(ctx), false))
  , mCurFunc(nullptr)
{
}

llvm::Module&
EmitIR::operator()(asg::TranslationUnit* tu)
{
  for (auto&& i : tu->decls)
    self(i);
  return mMod;
}

//==============================================================================
// 类型
//==============================================================================

llvm::Type*
EmitIR::operator()(const Type* type)
{
  if (type->texp == nullptr) {
    switch (type->spec) {
      case Type::Spec::kInt:
        return llvm::Type::getInt32Ty(mCtx);
      case Type::Spec::kVoid:
        return llvm::Type::getVoidTy(mCtx);
      default:
        ABORT();
    }
  }

  Type subt;
  subt.spec = type->spec;
  subt.qual = type->qual;
  subt.texp = type->texp->sub;

  if (auto p = type->texp->dcst<PointerType>()) {
    // return llvm::PointerType::get(self(&subt), 0);
    return self(&subt);
  }
  if (auto p = type->texp->dcst<ArrayType>()) {
    // TODO 统一
    return get_array_type(p);
  }
  if (auto p = type->texp->dcst<FunctionType>()) {
    std::vector<llvm::Type*> pty;
    for (auto param : p->params) {
      pty.push_back(self(param));
    }
    return llvm::FunctionType::get(self(&subt), pty, false);
  }

  ABORT();
}

//==============================================================================
// 表达式
//==============================================================================

llvm::Value*
EmitIR::operator()(Expr* obj)
{
  if (auto p = obj->dcst<IntegerLiteral>())
    return self(p);
  if (auto p = obj->dcst<ParenExpr>())
    return self(p);
  if (auto p = obj->dcst<UnaryExpr>())
    return self(p);
  if (auto p = obj->dcst<BinaryExpr>())
    return self(p);
  if (auto p = obj->dcst<CallExpr>())
    return self(p);
  if (auto p = obj->dcst<InitListExpr>())
    return self(p);
  if (auto p = obj->dcst<ImplicitInitExpr>())
    return nullptr;
  if (auto p = obj->dcst<ImplicitCastExpr>())
    return self(p);
  if (auto p = obj->dcst<DeclRefExpr>())
    return self(p);

  ABORT();
}

llvm::Constant*
EmitIR::operator()(IntegerLiteral* obj)
{
  return llvm::ConstantInt::get(self(obj->type), obj->val);
}

llvm::Value*
EmitIR::operator()(ParenExpr* obj)
{
  return self(obj->sub); // IR val
}

llvm::Value*
EmitIR::operator()(UnaryExpr* obj)
{
  auto val = self(obj->sub); // IR val

  auto &irb = *mCurIrb;
  switch (obj->op) {
    case UnaryExpr::kPos: {
      return val; // IR val
    }
    case UnaryExpr::kNeg: {
      return irb.CreateNeg(val); // IR val
    }
    case UnaryExpr::kNot: {
      auto condVal = get_cond_val(val);
      return irb.CreateNot(condVal); // IR val
    }

    default:
      ABORT();
  }
}

llvm::Value* 
EmitIR::get_array_indexed(BinaryExpr *indexExpr)
{
  // WARN: 强制处理跳过指针转换，可能有其他情况
  auto arr = indexExpr->lft->dcst<ImplicitCastExpr>()->sub; 
  auto idx = indexExpr->rht;
  auto arrVal = self(arr);
  auto idxVal = self(idx);

  auto &irb = *mCurIrb;
  if (auto p = arr->type->texp->dcst<PointerType>()) {
    auto ty = self(arr->type);
    std::vector<llvm::Value*> idxList{
      idxVal // WARN: 类型最好应该转为 i64
    };
    return irb.CreateInBoundsGEP(ty, arrVal, idxList);
  }
  else if (auto p = arr->type->texp->dcst<ArrayType>()){
    auto arrTy = get_array_type(p);
    std::vector<llvm::Value*> idxList{
      irb.getInt64(0),
      idxVal // WARN: 类型最好应该转为 i64
    };

    // WARN 暂时存在逻辑重复，但不影响结果正确性
    return irb.CreateInBoundsGEP(arrTy, arrVal, idxList);
  }
  else 
    ABORT();
}

llvm::Value*
EmitIR::short_circuit_and(BinaryExpr* obj)
{
  auto &irb = *mCurIrb;

  auto lftBb = llvm::BasicBlock::Create(mCtx, "and_lft", mCurFunc);
  auto rhtBb = llvm::BasicBlock::Create(mCtx, "and_rht", mCurFunc);
  auto exitBb = llvm::BasicBlock::Create(mCtx, "and_exit", mCurFunc);

  irb.CreateBr(lftBb);

  irb.SetInsertPoint(lftBb);
  auto lftVal = get_cond_val(self(obj->lft)); // IR val
  llvm::Value* result = irb.CreateAlloca(irb.getInt1Ty(), nullptr, "and_result");
  irb.CreateStore(lftVal, result);
  irb.CreateCondBr(lftVal, rhtBb, exitBb);

  irb.SetInsertPoint(rhtBb);
  auto rhtVal = get_cond_val(self(obj->rht)); // IR val
  irb.CreateStore(rhtVal, result);
  irb.CreateBr(exitBb);

  irb.SetInsertPoint(exitBb);
  return irb.CreateLoad(irb.getInt1Ty(), result);
}

llvm::Value*
EmitIR::short_circuit_or(BinaryExpr* obj)
{
  auto &irb = *mCurIrb;

  auto lftBb = llvm::BasicBlock::Create(mCtx, "or_lft", mCurFunc);
  auto rhtBb = llvm::BasicBlock::Create(mCtx, "or_rht", mCurFunc);
  auto exitBb = llvm::BasicBlock::Create(mCtx, "or_exit", mCurFunc);

  irb.CreateBr(lftBb);

  irb.SetInsertPoint(lftBb);
  auto lftVal = get_cond_val(self(obj->lft)); // IR val
  llvm::Value* result = irb.CreateAlloca(irb.getInt1Ty(), nullptr, "or_result");  // WARN 应该放在函数开头
  irb.CreateStore(lftVal, result);
  irb.CreateCondBr(lftVal, exitBb, rhtBb);

  irb.SetInsertPoint(rhtBb);
  auto rhtVal = get_cond_val(self(obj->rht)); // IR val
  irb.CreateStore(rhtVal, result);
  irb.CreateBr(exitBb);

  irb.SetInsertPoint(exitBb);
  return irb.CreateLoad(irb.getInt1Ty(), result);

  // irb.SetInsertPoint(lftBb);
  // auto lftVal = get_cond_val(self(obj->lft)); // IR val
  // irb.CreateCondBr(lftVal, exitBb, rhtBb);

  // irb.SetInsertPoint(rhtBb);
  // auto rhtVal = get_cond_val(self(obj->rht)); // IR val
  // irb.CreateBr(exitBb);

  // irb.SetInsertPoint(exitBb);
  // auto phi = irb.CreatePHI(irb.getInt1Ty(), 2);
  // phi->addIncoming(irb.getTrue(), lftBb);
  // phi->addIncoming(rhtVal, rhtBb);

  // return phi;
}

llvm::Value*
EmitIR::operator()(BinaryExpr* obj)
{
  if (obj->op == BinaryExpr::kAnd) {
    return short_circuit_and(obj);
  }
  if (obj->op == BinaryExpr::kOr) {
    return short_circuit_or(obj);
  }

  auto lftVal = self(obj->lft); // IR val
  auto rhtVal = self(obj->rht); // IR val

  auto &irb = *mCurIrb;
  switch (obj->op) {
    case BinaryExpr::kAssign: {
      return irb.CreateStore(rhtVal, lftVal); // IR inst (val)
    }
    case BinaryExpr::kAdd: {
      return irb.CreateAdd(lftVal, rhtVal); // IR val
    }
    case BinaryExpr::kSub: {
      return irb.CreateSub(lftVal, rhtVal); // IR val
    }
    case BinaryExpr::kMul: {
      return irb.CreateMul(lftVal, rhtVal); // IR val
    }
    case BinaryExpr::kDiv: {
      return irb.CreateSDiv(lftVal, rhtVal); // IR val
    }
    case BinaryExpr::kMod: {
      return irb.CreateSRem(lftVal, rhtVal); // IR val
    }
    case BinaryExpr::kGt: {
      return irb.CreateICmpSGT(lftVal, rhtVal); // IR val
    }
    case BinaryExpr::kLt: {
      return irb.CreateICmpSLT(lftVal, rhtVal); // IR val
    }
    case BinaryExpr::kGe: {
      return irb.CreateICmpSGE(lftVal, rhtVal); // IR val
    }
    case BinaryExpr::kLe: {
      return irb.CreateICmpSLE(lftVal, rhtVal); // IR val
    }
    case BinaryExpr::kEq: {
      return irb.CreateICmpEQ(lftVal, rhtVal); // IR val
    }
    case BinaryExpr::kNe: {
      return irb.CreateICmpNE(lftVal, rhtVal); // IR val
    }
    case BinaryExpr::kComma: {
      return rhtVal; // IR val
    }
    case BinaryExpr::kIndex: {
      return get_array_indexed(obj); // IR addr
    }

    default:
      ABORT();
  }
}

llvm::Value*
EmitIR::operator()(CallExpr* obj)
{
  auto funcName = self(obj->head)->getName();
  auto func = mMod.getFunction(funcName);
  std::vector<llvm::Value*> args;
  for (auto arg : obj->args) {
    args.push_back(self(arg));
  }
  return mCurIrb->CreateCall(func, args); // IR val
}

void
EmitIR::array_empty_filler(std::vector<llvm::Constant*> &arr, ArrayType* arrTyExpr)
{
  auto len = arrTyExpr->len;

  // 多维数组
  if (auto p = arrTyExpr->sub->dcst<ArrayType>()) {
    auto aty = get_array_type(p);
    
    while (arr.size() < len) {
      std::vector<llvm::Constant*> emptyArr;
      array_empty_filler(emptyArr, p);
      
      arr.push_back(llvm::ConstantArray::get(aty, emptyArr));
    }
  }
  else { // 一维数组
    auto ty = mIntTy;

    while (arr.size() < len) 
      arr.push_back(llvm::ConstantInt::get(ty, 0));
  }
}

llvm::Constant*
EmitIR::operator()(InitListExpr* obj)
{
  auto arrTyExpr = obj->type->texp->dcst<ArrayType>();
  auto arrTy = get_array_type(arrTyExpr);
  auto len = arrTyExpr->len;

  std::vector<llvm::Constant*> elements;
  auto runtimeVals = new std::vector<std::pair<int, llvm::Value*>>();

  int idx = 0;
  for (auto expr : obj->list) {
    llvm::Constant* val;
    if (auto p = expr->dcst<IntegerLiteral>()) {
      val = self(p);
    }
    else if (auto p = expr->dcst<InitListExpr>()) {
      val = self(p);
    }
    else if (auto p = expr->dcst<ImplicitInitExpr>()) {
      continue;
    }
    else { // 运行时表达式，一定是最底层，使用 0 填充留给后续 IR Store 处理
      val = llvm::ConstantInt::get(mIntTy, 0);
      runtimeVals->push_back({idx, self(expr)});
    }

    elements.push_back(val);
    idx++;
  }

  // runtimeVals 交给 void *
  obj->any = runtimeVals;

  // 补全
  if (elements.size() < len) {
    array_empty_filler(elements, arrTyExpr);
  }

  return llvm::ConstantArray::get(arrTy, elements);
}

llvm::Value*
EmitIR::operator()(ImplicitCastExpr* obj)
{
  auto subVal = self(obj->sub); // IR val

  auto &irb = *mCurIrb;
  switch (obj->kind) {
    case ImplicitCastExpr::kLValueToRValue: { // 地址 -> 值
      auto ty = self(obj->sub->type);
      return irb.CreateLoad(ty, subVal); // IR inst (val)
    }
    case ImplicitCastExpr::kArrayToPointerDecay: { // 地址 -> 地址
      return subVal; // IR val
    }
    case ImplicitCastExpr::kFunctionToPointerDecay: {
      return subVal; // IR val
    }

    default:
      ABORT();
  }
}

llvm::Value*
EmitIR::operator()(DeclRefExpr* obj)
{
  // 获取对应 declare 的 IR (gvar)
  return reinterpret_cast<llvm::Value*>(obj->decl->any);
}

// TODO: 在此添加对更多表达式类型的处理

//==============================================================================
// 语句
//==============================================================================

void
EmitIR::operator()(Stmt* obj)
{
  // TODO: 在此添加对更多Stmt类型的处理的跳转

  if (auto p = obj->dcst<CompoundStmt>())
    return self(p);

  if (auto p = obj->dcst<DeclStmt>())
    return self(p);

  if (auto p = obj->dcst<ExprStmt>())
    return self(p);

  if (auto p = obj->dcst<ReturnStmt>())
    return self(p);

  if (auto p = obj->dcst<IfStmt>())
    return self(p);

  if (auto p = obj->dcst<WhileStmt>())
    return self(p);

  if (auto p = obj->dcst<DoStmt>())
    return self(p);

  if (auto p = obj->dcst<BreakStmt>())
    return self(p);

  if (auto p = obj->dcst<ContinueStmt>())
    return self(p);

  ABORT();
}

void 
EmitIR::var_decl(VarDecl* decl)
{
  auto ty = llvm::Type::getInt32Ty(mCtx);

  //# 创建变量，分配内存
  goto_entry_block();
  mCurIrb->CreateAlloca(
    ty, nullptr, decl->name); // IR inst
  leave_entry_block();

  auto varVal = mCurFunc->getValueSymbolTable()->lookup(decl->name); // IR addr
  decl->any = varVal;

  //# 初始化构造器，直接原地插入 store 即可
  if (decl->init != nullptr)
    trans_init(varVal, decl->init); // IR inst 
}

llvm::ArrayType*
EmitIR::get_array_type(ArrayType *typeExpr)
{
  if (typeExpr->sub == nullptr) {
    auto ty = llvm::Type::getInt32Ty(mCtx);
    return llvm::ArrayType::get(
    ty,
    typeExpr->len);
  }
  return llvm::ArrayType::get(
  get_array_type(typeExpr->sub->dcst<ArrayType>()), 
  typeExpr->len);
}

// 从 obj->any 获取即将要运行时的列表赋值
void
get_array_runtime_dimsvals(
  std::vector<std::pair<std::vector<int>, llvm::Value*>> &dimsVals,
  InitListExpr* initExpr,
  std::vector<int> curDims=std::vector<int>() // 回溯用
)
{
  // 递归底层
  if (initExpr->any) {
    auto runtimeVals = (std::vector<std::pair<int, llvm::Value*>> *) initExpr->any;
    for (auto [idx, val] : *runtimeVals) {
      curDims.push_back(idx);
      dimsVals.push_back({curDims, val});
      curDims.pop_back();
    }
    delete runtimeVals;
    initExpr->any = nullptr;
  }

  // 传递
  if (initExpr->list[0]->dcst<InitListExpr>()) {
    int idx = 0;
    for (auto e : initExpr->list) {
      auto subInitExpr = e->dcst<InitListExpr>();
      curDims.push_back(idx);
      get_array_runtime_dimsvals(dimsVals, subInitExpr, curDims);
      curDims.pop_back();
      idx++;
    }
  }
}

// 使用运行时值的列表赋值
void
EmitIR::array_runtime_init(
  llvm::Value* arrVal, llvm::Type* arrTy,
  llvm::Value* rtVal, 
  InitListExpr* initExpr,
  std::vector<int> &dims)
{
  auto &irb = *mCurIrb;
  std::vector<llvm::Value*> idxList;
  idxList.push_back(irb.getInt64(0));
  for (auto idx : dims) {
    idxList.push_back(irb.getInt64(idx));
  }

  auto arrPtr = irb.CreateInBoundsGEP(arrTy, arrVal, idxList);
  irb.CreateStore(rtVal, arrPtr);
}

// 使用常量数组的列表复制 
void 
EmitIR::array_const_init(
  VarDecl* decl,
  InitListExpr* initExpr,
  llvm::Value* arrVal)
{
  auto constArr = self(initExpr); // IR const arr
  auto gConstArr = new llvm::GlobalVariable(
    mMod, constArr->getType(), true, llvm::GlobalValue::ExternalLinkage, 
    constArr, decl->name + ".init");

  auto gConstArrPtr = mCurIrb->CreatePointerCast(
    gConstArr, llvm::PointerType::getUnqual(constArr->getType()));

  mCurIrb->CreateMemCpy(
    arrVal, llvm::Align(4),
    gConstArrPtr, llvm::Align(4),
    constArr->getType()->getArrayNumElements() * 8  // 8:byte
  );
}

void
EmitIR::array_decl(VarDecl* decl)
{
  //# 创建数组
  auto aty = get_array_type(
    decl->type->texp->dcst<ArrayType>());
  
  goto_entry_block();
  mCurIrb->CreateAlloca(
    aty, nullptr, decl->name); // IR inst
  leave_entry_block();

  auto arrVal = mCurFunc->getValueSymbolTable()->lookup(decl->name); // IR addr
  decl->any = arrVal;

  //# 初始化构造器
  if (decl->init != nullptr) {
    if (auto initExpr = decl->init->dcst<InitListExpr>()) {
      array_const_init(decl, initExpr, arrVal);

      // 处理运行时值用于初始化：
      std::vector<std::pair<std::vector<int>, llvm::Value*>> rtDimsVals;
      get_array_runtime_dimsvals(rtDimsVals, initExpr);
      for (auto [dims, rtVal] : rtDimsVals) {
        array_runtime_init(arrVal, aty, rtVal, initExpr, dims);
      }
    }
    else 
      ABORT();
  }
}

// 可以用 p->type->spec 确定基本数据类型，暂时全只用 int
void
EmitIR::operator()(DeclStmt* obj)
{
  for (auto decl : obj->decls) {
    if (auto p = decl->dcst<VarDecl>()) 
    {
      auto typeExpr = p->type->texp;
      if (auto pty = typeExpr->dcst<ArrayType>()) {
        array_decl(p);
      }
      else {
        var_decl(p);
      }
    }
    else
      ABORT();
  }
}

void
EmitIR::operator()(ExprStmt* obj)
{
  self(obj->expr);
}

void
EmitIR::operator()(CompoundStmt* obj)
{
  // TODO: 可以在此添加对符号重名的处理
  for (auto&& stmt : obj->subs) 
    self(stmt);
}

llvm::Value*
EmitIR::get_cond_val(llvm::Value* condVal) // input IR val
{
  auto ty = condVal->getType();
  if (ty->isIntegerTy(1)) { 
    // bool (i1)
    return condVal;
  }
  else {
    auto &irb = *mCurIrb;
    auto zero = irb.CreateIntCast(
      llvm::ConstantInt::get(ty, 0), ty, false);
    return irb.CreateICmpNE(condVal, zero); // IR val
  }
}

void
EmitIR::operator()(IfStmt* obj)
{
  auto condVal = get_cond_val(self(obj->cond)); // IR val

  llvm::BasicBlock* thenBb = llvm::BasicBlock::Create(mCtx, "if_then", mCurFunc);
  llvm::BasicBlock* elseBb = 
    obj->else_ ? llvm::BasicBlock::Create(mCtx, "if_else", mCurFunc) : nullptr;
  llvm::BasicBlock* exitBb = llvm::BasicBlock::Create(mCtx, "if_exit", mCurFunc);

  if (obj->else_ == nullptr) {
    mCurIrb->CreateCondBr(condVal, thenBb, exitBb);

    mCurIrb = std::make_unique<llvm::IRBuilder<>>(thenBb);
    self(obj->then);
    if (!mCurIrb->GetInsertBlock()->getTerminator())
      mCurIrb->CreateBr(exitBb);
  }
  else {
    mCurIrb->CreateCondBr(condVal, thenBb, elseBb);

    mCurIrb = std::make_unique<llvm::IRBuilder<>>(thenBb);
    self(obj->then);
    if (!mCurIrb->GetInsertBlock()->getTerminator())
      mCurIrb->CreateBr(exitBb);

    mCurIrb = std::make_unique<llvm::IRBuilder<>>(elseBb);
    self(obj->else_);
    if (!mCurIrb->GetInsertBlock()->getTerminator())
      mCurIrb->CreateBr(exitBb);
  }

  mCurIrb = std::make_unique<llvm::IRBuilder<>>(exitBb);
}

void
EmitIR::operator()(WhileStmt* obj)
{
  auto condBb = llvm::BasicBlock::Create(mCtx, "while_cond", mCurFunc);
  auto bodyBb = llvm::BasicBlock::Create(mCtx, "while_body", mCurFunc);
  auto exitBb = llvm::BasicBlock::Create(mCtx, "while_exit", mCurFunc);

  // for break
  obj->any = exitBb;
  // for continue
  obj->cond->any = condBb;

  mCurIrb->CreateBr(condBb);

  mCurIrb = std::make_unique<llvm::IRBuilder<>>(condBb);
  auto condVal = get_cond_val(self(obj->cond)); // IR val
  mCurIrb->CreateCondBr(condVal, bodyBb, exitBb);

  mCurIrb = std::make_unique<llvm::IRBuilder<>>(bodyBb);
  self(obj->body);
  mCurIrb->CreateBr(condBb);

  mCurIrb = std::make_unique<llvm::IRBuilder<>>(exitBb);
}

void
EmitIR::operator()(DoStmt* obj)
{
  auto condBb = llvm::BasicBlock::Create(mCtx, "do_cond", mCurFunc);
  auto bodyBb = llvm::BasicBlock::Create(mCtx, "do_body", mCurFunc);
  auto exitBb = llvm::BasicBlock::Create(mCtx, "do_exit", mCurFunc);

  mCurIrb->CreateBr(bodyBb);

  mCurIrb = std::make_unique<llvm::IRBuilder<>>(bodyBb);
  self(obj->body);
  mCurIrb->CreateBr(condBb);

  mCurIrb = std::make_unique<llvm::IRBuilder<>>(condBb);
  auto condVal = get_cond_val(self(obj->cond)); // IR val
  mCurIrb->CreateCondBr(condVal, bodyBb, exitBb);

  mCurIrb = std::make_unique<llvm::IRBuilder<>>(exitBb);

  // for break
  obj->any = exitBb;
  // for continue
  obj->cond->any = condBb;
}

void
EmitIR::operator()(BreakStmt* obj)
{
  auto exitBb = (llvm::BasicBlock*) obj->loop->any;
  mCurIrb->CreateBr(exitBb); // IR inst
}

void 
EmitIR::operator()(ContinueStmt* obj)
{
  llvm::BasicBlock* condBb = nullptr;
  if (auto p = obj->loop->dcst<WhileStmt>())
    condBb = (llvm::BasicBlock*) p->cond->any;
  else if (auto p = obj->loop->dcst<DoStmt>())
    condBb = (llvm::BasicBlock*) p->cond->any;
  else
    ABORT();
  
  if (condBb)
    mCurIrb->CreateBr(condBb); // IR inst
}

void
EmitIR::operator()(ReturnStmt* obj)
{
  auto& irb = *mCurIrb;

  llvm::Value* retVal;
  if (!obj->expr)
    retVal = nullptr;
  else
    retVal = self(obj->expr);

  mCurIrb->CreateRet(retVal);

  auto exitBb = llvm::BasicBlock::Create(mCtx, "return_exit", mCurFunc);
  mCurIrb = std::make_unique<llvm::IRBuilder<>>(exitBb);
}

//==============================================================================
// 声明
//==============================================================================

void
EmitIR::operator()(Decl* obj)
{
  if (auto p = obj->dcst<VarDecl>())
    return self(p);

  if (auto p = obj->dcst<FunctionDecl>())
    return self(p);

  ABORT();
}

void
EmitIR::trans_init(llvm::Value* ptrVal, Expr* obj)
{
  auto& irb = *mCurIrb;

  // 整数 字面量 值初始化
  if (auto p = obj->dcst<IntegerLiteral>()) {
    auto initVal = llvm::ConstantInt::get(self(p->type), p->val);
    irb.CreateStore(initVal, ptrVal); // IR inst
    return;
  }
  if (auto p = obj->dcst<InitListExpr>()) {
    ABORT();
  }
  else {
    auto val = self(obj); // IR val
    irb.CreateStore(val, ptrVal); // IR inst
  }
}

void 
EmitIR::global_var_decl(VarDecl* decl)
{
  //# 创建全局变量 IR
  auto ty = mIntTy;
  auto gvar = new llvm::GlobalVariable(
    mMod, ty, false, 
    llvm::GlobalValue::ExternalLinkage, 
    nullptr, decl->name);
  gvar->setInitializer(llvm::ConstantInt::get(ty, 0));
  
  decl->any = gvar; // annotation for DeclRefExpr

  if (decl->init == nullptr)
    return;

  //# 初始化构造器
  goto_global_ctor_block(decl);

  // 创建 store 指令将表达式的值存入全局变量 gvar
  trans_init(gvar, decl->init); // IR inst
  
  leave_global_ctor_block();
}

void 
EmitIR::global_array_decl(VarDecl* decl)
{
  //# 创建全局数组
  auto aty = get_array_type(
    decl->type->texp->dcst<ArrayType>());
  auto gvar = new llvm::GlobalVariable(
    mMod, aty, false, 
    llvm::GlobalValue::ExternalLinkage, 
    nullptr, decl->name);
  gvar->setInitializer(llvm::ConstantAggregateZero::get(aty));
  
  decl->any = gvar; // annotation for DeclRefExpr

  if (decl->init == nullptr)
    return;

  //# 初始化构造器
  goto_global_ctor_block(decl);

  // 创建 store 指令将表达式的值存入全局变量 gvar
  if (auto initExpr = decl->init->dcst<InitListExpr>()) {
    array_const_init(decl, initExpr, gvar);

    // // 全局数组不应有运行时值初始化
    // std::vector<std::pair<std::vector<int>, llvm::Value*>> rtDimsVals;
    // get_array_runtime_dimsvals(rtDimsVals, initExpr);
    // if (!rtDimsVals.empty()) 
    //   ABORT();
  }
  else 
    ABORT();

  leave_global_ctor_block();
}

void 
EmitIR::operator()(VarDecl* obj) // 变量声明
{
  auto typeExpr = obj->type->texp;
  if (auto p = typeExpr->dcst<ArrayType>()) {
    global_array_decl(obj);
  }
  else {
    global_var_decl(obj);
  }
}


void
EmitIR::operator()(FunctionDecl* obj)
{
  // 创建函数
  auto fty = llvm::dyn_cast<llvm::FunctionType>(self(obj->type));
  auto func = llvm::Function::Create(
    fty, llvm::GlobalVariable::ExternalLinkage, obj->name, mMod);

  obj->any = func;

  if (obj->body == nullptr)
    return;
  auto entryBb = llvm::BasicBlock::Create(mCtx, "entry", func);
  mCurIrb = std::make_unique<llvm::IRBuilder<>>(entryBb);
  auto& entryIrb = *mCurIrb;

  // 对函数参数的处理：局部参数声明
  auto argIter = func->arg_begin();
  for (auto param : obj->params) {
    auto argVal = &*argIter;
    argVal->setName(param->name);
    argIter++;

    // 忽略默认值？
    auto ty = self(param->type);
    auto argPtr = entryIrb.CreateAlloca(ty, nullptr, param->name + ".addr");
    entryIrb.CreateStore(argVal, argPtr);
    param->any = argPtr;
  }

  // 翻译函数体
  mCurFunc = func;
  self(obj->body);
  auto& exitIrb = *mCurIrb;

  if (fty->getReturnType()->isVoidTy())
    exitIrb.CreateRetVoid();
  else
    exitIrb.CreateUnreachable();

  mCurFunc = nullptr; // 函数外，全局内容
}
