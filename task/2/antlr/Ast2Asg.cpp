#include "Ast2Asg.hpp"
#include "asg.hpp"
#include <unordered_map>

#define self (*this)

namespace asg {

// 符号表，保存当前作用域的所有声明
struct Ast2Asg::Symtbl : public std::unordered_map<std::string, Decl*>
{
  Ast2Asg& m;
  Symtbl* mPrev;

  Symtbl(Ast2Asg& m)
    : m(m)
    , mPrev(m.mSymtbl)
  {
    m.mSymtbl = this;
  }

  ~Symtbl() { m.mSymtbl = mPrev; }

  Decl* resolve(const std::string& name);
};

Decl*
Ast2Asg::Symtbl::resolve(const std::string& name)
{
  auto iter = find(name);
  if (iter != end())
    return iter->second;
  ASSERT(mPrev != nullptr); // 标识符未定义
  return mPrev->resolve(name);
}

TranslationUnit*
Ast2Asg::operator()(ast::TranslationUnitContext* ctx)
{
  auto ret = make<asg::TranslationUnit>();
  if (ctx == nullptr)
    return ret;

  Symtbl localDecls(self);

  for (auto&& i : ctx->externalDeclaration()) {
    if (auto p = i->declaration()) {
      auto decls = self(p);
      ret->decls.insert(ret->decls.end(),
                        std::make_move_iterator(decls.begin()),
                        std::make_move_iterator(decls.end()));
    }

    else if (auto p = i->functionDefinition()) {
      auto funcDecl = self(p);
      ret->decls.push_back(funcDecl);

      // 添加到声明表
      localDecls[funcDecl->name] = funcDecl;
    }

    else if (auto p = i->functionDeclaration()) {
      auto funcDecl = self(p);
      ret->decls.push_back(funcDecl);

      // 添加到声明表
      localDecls[funcDecl->name] = funcDecl;
    } 

    else
      ABORT();
  }

  return ret;
}

//==============================================================================
// 类型
//==============================================================================

Ast2Asg::SpecQual
Ast2Asg::operator()(ast::DeclarationSpecifiersContext* ctx)
{
  SpecQual ret = { Type::Spec::kINVALID, Type::Qual() };

  for (auto&& i : ctx->declarationSpecifier()) {
    if (auto p = i->typeSpecifier()) {
      if (p->Int())
        ret.first = Type::Spec::kInt;
      
      else if (p->Void())
        ret.first = Type::Spec::kVoid;
      
      else
        ABORT(); // 未知的类型说明符
    }

    else
      ABORT();
  }

  if (auto i = ctx->declarationQualifier()) {
    if (auto p = i->typeQualifier()) {
      if (p->Const())
        ret.second.const_ = true;
      else
        ABORT(); // 未知的类型限定符
    }
  }

  return ret;
}

std::pair<TypeExpr*, std::string>
Ast2Asg::operator()(ast::DeclaratorContext* ctx, TypeExpr* sub)
{
  return self(ctx->directDeclarator(), sub);
}

static int
eval_arrlen(Expr* expr)
{
  if (auto p = expr->dcst<IntegerLiteral>())
    return p->val;

  if (auto p = expr->dcst<DeclRefExpr>()) {
    if (p->decl == nullptr)
      ABORT();

    auto var = p->decl->dcst<VarDecl>();
    if (!var || !var->type->qual.const_)
      ABORT(); // 数组长度必须是编译期常量

    switch (var->type->spec) {
      case Type::Spec::kChar:
      case Type::Spec::kInt:
      case Type::Spec::kLong:
      case Type::Spec::kLongLong:
        return eval_arrlen(var->init);

      default:
        ABORT(); // 长度表达式必须是数值类型
    }
  }

  if (auto p = expr->dcst<UnaryExpr>()) {
    auto sub = eval_arrlen(p->sub);

    switch (p->op) {
      case UnaryExpr::kPos:
        return sub;

      case UnaryExpr::kNeg:
        return -sub;

      default:
        ABORT();
    }
  }

  if (auto p = expr->dcst<BinaryExpr>()) {
    auto lft = eval_arrlen(p->lft);
    auto rht = eval_arrlen(p->rht);

    switch (p->op) {
      case BinaryExpr::kAdd:
        return lft + rht;

      case BinaryExpr::kSub:
        return lft - rht;

      default:
        ABORT();
    }
  }

  if (auto p = expr->dcst<InitListExpr>()) {
    if (p->list.empty())
      return 0;
    return eval_arrlen(p->list[0]);
  }

  ABORT();
}

std::pair<TypeExpr*, std::string>
Ast2Asg::operator()(ast::DirectDeclaratorContext* ctx, TypeExpr* sub)
{
  if (auto p = ctx->Identifier())
    return { sub, p->getText() };

  // Bracket
  if (ctx->L_Square()) {
    auto arrayType = make<ArrayType>();
    arrayType->sub = sub;

    if (auto p = ctx->assignmentExpression())
      arrayType->len = eval_arrlen(self(p));
    else
      arrayType->len = ArrayType::kUnLen;

    return self(ctx->directDeclarator(), arrayType);
  }

  ABORT();
}

//==============================================================================
// 表达式
//==============================================================================

Expr*
Ast2Asg::operator()(ast::ExpressionContext* ctx)
{
  auto list = ctx->assignmentExpression();
  Expr* ret = self(list[0]);

  for (unsigned i = 1; i < list.size(); ++i) {
    auto node = make<BinaryExpr>();
    node->op = node->kComma;
    node->lft = ret;
    node->rht = self(list[i]);
    ret = node;
  }

  return ret;
}

Expr*
Ast2Asg::operator()(ast::AssignmentExpressionContext* ctx)
{
  if (auto p = ctx->logicalOrExpression())
    return self(p);

  auto ret = make<BinaryExpr>();
  ret->op = ret->kAssign;
  ret->lft = self(ctx->unaryExpression());
  ret->rht = self(ctx->assignmentExpression());
  return ret;
}

Expr*
Ast2Asg::operator()(ast::LogicalOrExpressionContext* ctx)
{
  auto children = ctx->children;
  Expr* ret = self(dynamic_cast<ast::LogicalAndExpressionContext*>(children[0]));

  for (unsigned i = 1; i < children.size(); ++i) {
    auto node = make<BinaryExpr>();

    auto token = dynamic_cast<antlr4::tree::TerminalNode*>(children[i])
                   ->getSymbol()
                   ->getType();
    switch (token) {
      case ast::PipePipe:
        node->op = node->kOr;
        break;

      default:
        ABORT();
    }

    node->lft = ret;
    node->rht = 
      self(dynamic_cast<ast::LogicalAndExpressionContext*>(children[++i]));
    ret = node;
  }

  return ret;
}

Expr*
Ast2Asg::operator()(ast::LogicalAndExpressionContext* ctx)
{
  auto children = ctx->children;
  Expr* ret = self(dynamic_cast<ast::EquativeExpressionContext*>(children[0]));

  for (unsigned i = 1; i < children.size(); ++i) {
    auto node = make<BinaryExpr>();

    auto token = dynamic_cast<antlr4::tree::TerminalNode*>(children[i])
                   ->getSymbol()
                   ->getType();
    switch (token) {
      case ast::AmpAmp:
        node->op = node->kAnd;
        break;

      default:
        ABORT();
    }

    node->lft = ret;
    node->rht = 
      self(dynamic_cast<ast::EquativeExpressionContext*>(children[++i]));
    ret = node;
  }

  return ret;
}

Expr* 
Ast2Asg::operator()(ast::EquativeExpressionContext* ctx)
{
  auto children = ctx->children;
  Expr* ret = self(dynamic_cast<ast::ComparativeExpressionContext*>(children[0]));

  for (unsigned i = 1; i < children.size(); ++i) {
    auto node = make<BinaryExpr>();

    auto token = dynamic_cast<antlr4::tree::TerminalNode*>(children[i])
                   ->getSymbol()
                   ->getType();
    switch (token) {
      case ast::EqualEqual:
        node->op = node->kEq;
        break;

      case ast::ExclaimEqual:
        node->op = node->kNe;
        break;

      default:
        ABORT();
    }

    node->lft = ret;
    node->rht = 
      self(dynamic_cast<ast::ComparativeExpressionContext*>(children[++i]));
    ret = node;
  }

  return ret;
}

Expr*
Ast2Asg::operator()(ast::ComparativeExpressionContext* ctx)
{
  auto children = ctx->children;
  Expr* ret = self(dynamic_cast<ast::AdditiveExpressionContext*>(children[0]));

  for (unsigned i = 1; i < children.size(); ++i) {
    auto node = make<BinaryExpr>();

    auto token = dynamic_cast<antlr4::tree::TerminalNode*>(children[i])
                   ->getSymbol()
                   ->getType();
    switch (token) {
      case ast::Greater:
        node->op = node->kGt;
        break;

      case ast::Less:
        node->op = node->kLt;
        break;

      case ast::GreaterEqual:
        node->op = node->kGe;
        break;

      case ast::LessEqual:
        node->op = node->kLe;
        break;

      default:
        ABORT();
    }

    node->lft = ret;
    node->rht = 
      self(dynamic_cast<ast::AdditiveExpressionContext*>(children[++i]));
    ret = node;
  }

  return ret;
}

Expr*
Ast2Asg::operator()(ast::AdditiveExpressionContext* ctx)
{
  auto children = ctx->children;
  // assert(dynamic_cast<ast::UnaryExpressionContext*>(children[0]));
  Expr* ret =
    self(dynamic_cast<ast::MultiplicativeExpressionContext*>(children[0]));

  for (unsigned i = 1; i < children.size(); ++i) {
    auto node = make<BinaryExpr>();

    auto token = dynamic_cast<antlr4::tree::TerminalNode*>(children[i])
                   ->getSymbol()
                   ->getType();
    switch (token) {
      case ast::Plus:
        node->op = node->kAdd;
        break;

      case ast::Minus:
        node->op = node->kSub;
        break;

      default:
        ABORT();
    }

    node->lft = ret;
    node->rht = 
      self(dynamic_cast<ast::MultiplicativeExpressionContext*>(children[++i]));
    ret = node;
  }

  return ret;
}

Expr*
Ast2Asg::operator()(ast::MultiplicativeExpressionContext* ctx)
{
  auto children = ctx->children;
  Expr* ret = self(dynamic_cast<ast::UnaryExpressionContext*>(children[0]));

  for (unsigned i = 1; i < children.size(); ++i) {
    auto node = make<BinaryExpr>();

    auto token = dynamic_cast<antlr4::tree::TerminalNode*>(children[i])
                   ->getSymbol()
                   ->getType();
    switch (token) {
      case ast::Star:
        node->op = node->kMul;
        break;

      case ast::Slash:
        node->op = node->kDiv;
        break;

      case ast::Percent:
        node->op = node->kMod;
        break;

      default:
        ABORT();
    }

    node->lft = ret;
    node->rht = self(dynamic_cast<ast::UnaryExpressionContext*>(children[++i]));
    ret = node;
  }

  return ret;
}

Expr*
Ast2Asg::operator()(ast::UnaryExpressionContext* ctx)
{
  if (auto p = ctx->postfixExpression())
    return self(p);

  auto ret = make<UnaryExpr>();

  switch (
    dynamic_cast<antlr4::tree::TerminalNode*>(ctx->unaryOperator()->children[0])
      ->getSymbol()
      ->getType()) {
    case ast::Plus:
      ret->op = ret->kPos;
      break;

    case ast::Minus:
      ret->op = ret->kNeg;
      break;

    case ast::Exclaim:
      ret->op = ret->kNot;
      break;

    default:
      ABORT();
  }

  ret->sub = self(ctx->unaryExpression());

  return ret;
}

Expr*
Ast2Asg::operator()(ast::PostfixExpressionContext* ctx)
{
  if (auto p = ctx->primaryExpression())
    return self(p);


  if (auto p = ctx->postfixExpression()) {
    auto op = ctx->postfixOperator();

    // Support: Array index
    if (auto l = op->L_Square()) {
      auto ret = make<BinaryExpr>();
      ret->op = ret->kIndex;
      ret->lft = self(p);
      ret->rht = self(op->expression());
      return ret;
    }

    // Support: Function call
    if (auto l = op->L_Paren()) {
      auto ret = make<CallExpr>();
      ret->head = self(p);

      if (auto q = op->argumentExpressionList()) {
        for (auto&& i : q->assignmentExpression())
          ret->args.push_back(self(i));
      }

      return ret;
    }

    ABORT(); // 未知的后缀表达式
  }

  ABORT();
}

Expr*
Ast2Asg::operator()(ast::PrimaryExpressionContext* ctx)
{

  if (auto p = ctx->Identifier()) {
    auto name = p->getText();
    auto ret = make<DeclRefExpr>();
    ret->decl = mSymtbl->resolve(name);
    return ret;
  }

  if (auto p = ctx->Numeric_Constant()) {
    auto text = p->getText();

    auto ret = make<IntegerLiteral>();

    ASSERT(!text.empty());
    if (text[0] != '0')
      ret->val = std::stoll(text);

    else if (text.size() == 1)
      ret->val = 0;

    else if (text[1] == 'x' || text[1] == 'X')
      ret->val = std::stoll(text.substr(2), nullptr, 16);

    else
      ret->val = std::stoll(text.substr(1), nullptr, 8);

    return ret;
  }

  if (auto p = ctx->expression()) {
    auto ret = make<ParenExpr>();
    ret->sub = self(p);
    return ret;
  }

  ABORT();
}

Expr*
Ast2Asg::operator()(ast::InitializerContext* ctx)
{
  if (auto p = ctx->assignmentExpression())
    return self(p);

  auto ret = make<InitListExpr>();

  if (auto p = ctx->initializerList()) {
    for (auto&& i : p->initializer()) {
      // 将初始化列表展平
      auto expr = self(i);
      if (auto p = expr->dcst<InitListExpr>()) {
        for (auto&& sub : p->list)
          ret->list.push_back(sub);
      } else {
        ret->list.push_back(expr);
      }
    }
  }

  return ret;
}

//==============================================================================
// 语句
//==============================================================================

Stmt*
Ast2Asg::operator()(ast::StatementContext* ctx)
{
  if (auto p = ctx->compoundStatement())
    return self(p);

  if (auto p = ctx->expressionStatement())
    return self(p);

  if (auto p = ctx->conditionStatement())
    return self(p);

  if (auto p = ctx->jumpStatement())
    return self(p);

  ABORT();
}

CompoundStmt*
Ast2Asg::operator()(ast::CompoundStatementContext* ctx)
{
  auto ret = make<CompoundStmt>();

  if (auto p = ctx->blockItemList()) {
    Symtbl localDecls(self);

    for (auto&& i : p->blockItem()) {
      if (auto q = i->declaration()) {
        auto sub = make<DeclStmt>();
        sub->decls = self(q);
        ret->subs.push_back(sub);
      }

      else if (auto q = i->statement())
        ret->subs.push_back(self(q));

      else
        ABORT();
    }
  }

  return ret;
}

Stmt*
Ast2Asg::operator()(ast::ExpressionStatementContext* ctx)
{
  if (auto p = ctx->expression()) {
    auto ret = make<ExprStmt>();
    ret->expr = self(p);
    return ret;
  }

  return make<NullStmt>();
}

Stmt*
Ast2Asg::operator()(ast::ConditionStatementContext* ctx)
{
  // if
  if (auto p = ctx->If()) {
    auto ret = make<IfStmt>();

    ret->cond = self(ctx->expression());
    ret->then = self(ctx->statement(0));

    // else
    if (auto q = ctx->Else()) {
      auto i = ctx->statement(1);
      ret->else_ = self(i);
    }

    return ret;
  }

  // while
  if (auto p = ctx->While()) {
    auto ret = make<WhileStmt>();

    ret->cond = self(ctx->expression());
    ret->body = self(ctx->statement(0));

    return ret;
  }

  ABORT();
}

Stmt*
Ast2Asg::operator()(ast::JumpStatementContext* ctx)
{
  if (ctx->Return()) {
    auto ret = make<ReturnStmt>();
    ret->func = mCurrentFunc;
    if (auto p = ctx->expression())
      ret->expr = self(p);
    return ret;
  }

  if (ctx->Break()) {
    auto ret = make<BreakStmt>();
    return ret;
  }

  if (ctx->Continue()) {
    auto ret = make<ContinueStmt>();
    return ret;
  }

  ABORT();
}

//==============================================================================
// 声明
//==============================================================================

std::vector<Decl*>
Ast2Asg::operator()(ast::DeclarationContext* ctx)
{
  std::vector<Decl*> ret;

  if (auto p = ctx->declarationSpecifiers()) {
    auto specs = self(p);

    if (auto p = ctx->initDeclaratorList()) {
      for (auto&& j : p->initDeclarator())
        ret.push_back(self(j, specs));
    }
  }

  return ret;
}

FunctionDecl*
Ast2Asg::operator()(ast::FunctionDefinitionContext* ctx)
{
  auto ret = self(ctx->functionPrototype());
  mCurrentFunc = ret;

  // 外部符号表在 externalDeclaration 处理，续上本地的新符号表
  Symtbl localDecls(self); 

  // 加入本地符号表以允许递归调用
  (*mSymtbl)[ret->name] = ret;

  if (ret->body) // 重复定义
    ABORT();
  ret->body = self(ctx->compoundStatement());

  return ret;
}

FunctionDecl*
Ast2Asg::operator()(ast::FunctionDeclarationContext* ctx)
{
  auto ret = self(ctx->functionPrototype());

  // 外部符号表在 externalDeclaration 处理

  return ret;
}

FunctionDecl*
Ast2Asg::operator()(ast::FunctionPrototypeContext* ctx)
{
  auto ret = make<FunctionDecl>();
  auto type = make<Type>();
  auto funcType = make<FunctionType>();

  ret->type = type;
  type->texp = funcType;

  // function return type / qualifiers
  auto sq = self(ctx->declarationSpecifiers());
  type->spec = sq.first, type->qual = sq.second;

  // function name
  auto [texp, name] = self(ctx->directDeclarator(), nullptr);
  funcType->sub = texp; // 目前暂时为 nullptr
  ret->name = std::move(name);

  // function params
  if (auto p = ctx->parameterDeclarationList()) {
    for (auto&& i : p->parameterDeclaration()) {
      auto param = self(i);
      funcType->params.push_back(param->type);
      ret->params.push_back(param);
    }
  }

  return ret;
}

Decl*
Ast2Asg::operator()(ast::ParameterDeclarationContext* ctx)
{
  auto specs = self(ctx->declarationSpecifiers());
  Decl* ret = self(ctx->initDeclarator(), specs);

  return ret;
}

Decl*
Ast2Asg::operator()(ast::InitDeclaratorContext* ctx, SpecQual sq)
{
  auto [texp, name] = self(ctx->declarator(), nullptr);
  Decl* ret;

  // Function declare
  if (auto funcType = texp->dcst<FunctionType>()) {
    auto fdecl = make<FunctionDecl>();
    auto type = make<Type>();
    fdecl->type = type;

    type->spec = sq.first;
    type->qual = sq.second;
    type->texp = funcType;

    fdecl->name = std::move(name);
    for (auto p : funcType->params) {
      auto paramDecl = make<VarDecl>();
      paramDecl->type = p;
      fdecl->params.push_back(paramDecl);
    }

    if (ctx->initializer())
      ABORT();
    fdecl->body = nullptr;

    ret = fdecl;
  }

  // Variable declare
  else {
    auto vdecl = make<VarDecl>();
    auto type = make<Type>();
    vdecl->type = type;

    type->spec = sq.first;
    type->qual = sq.second;
    type->texp = texp;
    vdecl->name = std::move(name);

    if (auto p = ctx->initializer())
      vdecl->init = self(p);
    else
      vdecl->init = nullptr;

    ret = vdecl;
  }

  // 这个实现允许符号重复定义，新定义会取代旧定义
  (*mSymtbl)[ret->name] = ret;
  return ret;
}

} // namespace asg
