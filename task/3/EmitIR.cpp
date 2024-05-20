#include "EmitIR.hpp"
#include "Obj.hpp"
#include "asg.hpp"
#include <llvm/Transforms/Utils/ModuleUtils.h>

#include <llvm/IR/ValueSymbolTable.h>

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
      // TODO: 在此添加对更多基础类型的处理
      default:
        ABORT();
    }
  }

  Type subt;
  subt.spec = type->spec;
  subt.qual = type->qual;
  subt.texp = type->texp->sub;

  // TODO: 在此添加对指针类型、数组类型和函数类型的处理

  if (auto p = type->texp->dcst<FunctionType>()) {
    std::vector<llvm::Type*> pty;
    // TODO: 在此添加对函数参数类型的处理
    return llvm::FunctionType::get(self(&subt), std::move(pty), false);
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
  if (auto p = obj->dcst<BinaryExpr>())
    return self(p);
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
EmitIR::operator()(BinaryExpr* obj)
{
  auto lftVal = self(obj->lft); // IR val
  auto rhtVal = self(obj->rht); // IR val

  auto &irb = *mCurIrb;
  switch (obj->op) {
    case BinaryExpr::kAdd: {
      return irb.CreateAdd(lftVal, rhtVal); // IR inst
    }
  
    default:
      ABORT();
  }
}

llvm::Value*
EmitIR::operator()(ImplicitCastExpr* obj)
{
  auto subVal = self(obj->sub); // IR val

  auto &irb = *mCurIrb;
  switch (obj->kind) {
    case ImplicitCastExpr::kLValueToRValue: { // 子表达式左值（地址） -> 值
      auto ty = self(obj->sub->type);
      return irb.CreateLoad(ty, subVal); // IR inst
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

  if (auto p = obj->dcst<ReturnStmt>())
    return self(p);

  ABORT();
}

void
EmitIR::operator()(DeclStmt* obj)
{
  for (auto&& decl : obj->decls) {
    if (auto p = decl->dcst<VarDecl>()) {
      //# 创建局部变量 alloc IR
      auto curBlock = mCurIrb->GetInsertBlock(); // 暂时切换

      // 所有局部变量 alloc 插入到函数入口
      { 
        auto &entryBb = mCurFunc->getEntryBlock();
        mCurIrb = std::make_unique<llvm::IRBuilder<>>(&entryBb);
        
        auto ty = mIntTy; 
        mCurIrb->CreateAlloca(
          ty, nullptr, p->name); // IR inst

        p->any = mCurFunc->getValueSymbolTable()->lookup(p->name); // IR alloca addr
      }

      mCurIrb = std::make_unique<llvm::IRBuilder<>>(curBlock); // 回溯

      if (p->init == nullptr)
        return;

      //# 初始化构造器，直接原地插入 store 即可
      trans_init(reinterpret_cast<llvm::Value*>(p->any), p->init); // IR inst
    }
    else
      ABORT();
  }
}

void
EmitIR::operator()(CompoundStmt* obj)
{
  // TODO: 可以在此添加对符号重名的处理
  for (auto&& stmt : obj->subs) 
    self(stmt);
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

  ABORT();
}

void 
EmitIR::operator()(VarDecl* obj) // 变量声明
{
  //# 创建全局变量 IR
  auto ty = mIntTy;
  auto gvar = new llvm::GlobalVariable(
    mMod, ty, false, llvm::GlobalValue::ExternalLinkage, 
    nullptr, obj->name);
  gvar->setInitializer(llvm::ConstantInt::get(ty, 0));
  
  obj->any = gvar; // annotation for DeclRefExpr

  if (obj->init == nullptr)
    return;

  //# 初始化构造器
  mCurFunc = llvm::Function::Create(
    mCtorTy, llvm::GlobalVariable::ExternalLinkage, 
    obj->name + ".ctor", mMod); 
  
  auto entryBb = llvm::BasicBlock::Create(
    mCtx, "entry", mCurFunc); 
  // 指定当前IR插入点为 Block 的末尾
  mCurIrb = std::make_unique<llvm::IRBuilder<>>(entryBb); 
  // 创建 store 指令将表达式的值存入全局变量 gvar
  trans_init(gvar, obj->init); // IR inst
  // 将构造器添加到全局构造器列表中
  llvm::appendToGlobalCtors(mMod, mCurFunc, 65535);

  mCurIrb->CreateRet(nullptr);
  mCurFunc = nullptr;

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

  // TODO: 添加对函数参数的处理

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
