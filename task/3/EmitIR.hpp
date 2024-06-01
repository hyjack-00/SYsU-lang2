#include "asg.hpp"
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>

#include <llvm/Transforms/Utils/ModuleUtils.h>

class EmitIR
{
public:
  Obj::Mgr& mMgr;
  llvm::Module mMod;


  EmitIR(Obj::Mgr& mgr, llvm::LLVMContext& ctx, llvm::StringRef mid = "-");

  llvm::Module& operator()(asg::TranslationUnit* tu);

private:
  llvm::LLVMContext& mCtx;

  llvm::Type* mIntTy;
  llvm::FunctionType* mCtorTy;

  llvm::Function* mCurFunc;
  std::unique_ptr<llvm::IRBuilder<>> mCurIrb;
  llvm::BasicBlock* mPrevBb;

  //============================================================================
  // 类型
  //============================================================================

  llvm::Type* operator()(const asg::Type* type);

  //============================================================================
  // 表达式
  //============================================================================

  llvm::Value* operator()(asg::Expr* obj);

  llvm::Constant* operator()(asg::IntegerLiteral* obj);

  llvm::Value* operator()(asg::ParenExpr* obj);

  llvm::Value* operator()(asg::UnaryExpr* obj);

  llvm::Value* operator()(asg::BinaryExpr* obj);

  llvm::Value* operator()(asg::CallExpr* obj);

  llvm::Constant* operator()(asg::InitListExpr* obj);
  
  llvm::Value* operator()(asg::ImplicitCastExpr* obj);

  llvm::Value* operator()(asg::DeclRefExpr* obj);

  //============================================================================
  // 语句
  //============================================================================

  void operator()(asg::Stmt* obj);

  void operator()(asg::DeclStmt* obj);

  void operator()(asg::ExprStmt* obj);

  void operator()(asg::CompoundStmt* obj);

  void operator()(asg::IfStmt* obj);

  void operator()(asg::WhileStmt* obj);

  void operator()(asg::DoStmt* obj);

  void operator()(asg::BreakStmt* obj);

  void operator()(asg::ContinueStmt* obj);

  void operator()(asg::ReturnStmt* obj);

  // TODO: 添加语句处理相关声明

  //============================================================================
  // 声明
  //============================================================================


  void operator()(asg::Decl* obj);

  void operator()(asg::VarDecl* obj);
  
  void operator()(asg::FunctionDecl* obj);
  


  llvm::ArrayType* get_array_type(asg::ArrayType *typeExpr);
  llvm::Value* get_array_indexed(asg::BinaryExpr *indexExpr);
  void array_empty_filler(
    std::vector<llvm::Constant*> &arr, asg::ArrayType* arrTyExpr);
  void array_runtime_init(
    llvm::Value* arrVal, llvm::Type* arrTy,
    llvm::Value* rtVal, 
    asg::InitListExpr* initExpr,
    std::vector<int> &dims
  );
  void array_const_init(
    asg::VarDecl* decl,
    asg::InitListExpr* initExpr,
    llvm::Value* arrVal
  );
  
  void var_decl(asg::VarDecl* decl);
  void array_decl(asg::VarDecl* decl);

  void global_var_decl(asg::VarDecl* decl);
  void global_array_decl(asg::VarDecl* decl);

  void trans_init(llvm::Value* ptrVal, asg::Expr* obj);


  llvm::Value* get_cond_val(llvm::Value* cond);

  llvm::Value* short_circuit_and(asg::BinaryExpr* obj);
  llvm::Value* short_circuit_or(asg::BinaryExpr* obj);


  //============================================================================

  // for local alloc 
  void goto_entry_block() {
    // 所有局部变量 alloc 插入到函数入口
    mPrevBb = mCurIrb->GetInsertBlock(); // 暂时切换
    auto &entryBbRef = mCurFunc->getEntryBlock();
    mCurIrb->SetInsertPoint(&entryBbRef, entryBbRef.getFirstInsertionPt());
  }
  void leave_entry_block() {
    mCurIrb->SetInsertPoint(mPrevBb);
  }

  // for global alloc
  void goto_global_ctor_block(asg::VarDecl* decl) {
    mCurFunc = llvm::Function::Create(
      mCtorTy, llvm::GlobalVariable::ExternalLinkage, 
      decl->name + ".ctor", mMod); 
    
    auto entryBb = llvm::BasicBlock::Create(
      mCtx, "entry", mCurFunc); 
    // 指定当前IR插入点为 Block 的末尾
    mCurIrb = std::make_unique<llvm::IRBuilder<>>(entryBb);
    // 将构造器添加到全局构造器列表中
    llvm::appendToGlobalCtors(mMod, mCurFunc, 65535);
  }
  void leave_global_ctor_block() {
    mCurIrb->CreateRet(nullptr);
    mCurFunc = nullptr;
  }
};
