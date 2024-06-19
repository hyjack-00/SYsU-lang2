#include "AlgebraicIdentities.hpp"

using namespace llvm;

/*
x + 0
0 + x
x - 0
0 - x
1 * x
x * 1
0 * x
x * 0
x / 1
0 / x
x % 1
*/

PreservedAnalyses
AlgebraicIdentities::run(Module& mod, ModuleAnalysisManager& mam)
{
  int algebraicIdentitiesTimes = 0;

  for (auto& func : mod) {
    for (auto& bb : func) {
      for (auto& inst : bb) {
        if (auto binOp = dyn_cast<BinaryOperator>(&inst)) {
          Value* lhs = binOp->getOperand(0);
          Value* rhs = binOp->getOperand(1);
          auto constLhs = dyn_cast<ConstantInt>(lhs);
          auto constRhs = dyn_cast<ConstantInt>(rhs);

          switch (binOp->getOpcode()) {
            // 1. 加法恒等式
            case Instruction::Add: {
              if (constLhs && constLhs->isZero()) {
                // 0 + x -> x
                binOp->replaceAllUsesWith(rhs);
                ++algebraicIdentitiesTimes;
              } 
              else if (constRhs && constRhs->isZero()) {
                // x + 0 -> x
                binOp->replaceAllUsesWith(lhs);
                ++algebraicIdentitiesTimes;
              }
              break;
            }
            // 2. 减法恒等式
            case Instruction::Sub: {
              if (constRhs && constRhs->isZero()) {
                // x - 0 -> x
                binOp->replaceAllUsesWith(lhs);
                ++algebraicIdentitiesTimes;
              }
              if (constLhs && constLhs->isZero()) {
                // 0 - x -> -x
                binOp->replaceAllUsesWith(BinaryOperator::CreateNeg(
                  rhs, 
                  "", 
                  &inst));
                ++algebraicIdentitiesTimes;
              }
              break;
            }
            // 3. 乘法恒等式
            case Instruction::Mul: {
              if (constLhs && constLhs->isOne()) {
                // 1 * x -> x
                binOp->replaceAllUsesWith(rhs);
                ++algebraicIdentitiesTimes;
              }
              else if (constRhs && constRhs->isOne()) {
                // x * 1 -> x
                binOp->replaceAllUsesWith(lhs);
                ++algebraicIdentitiesTimes;
              }
              else if (constLhs && constLhs->isZero()) {
                // 0 * x -> 0
                binOp->replaceAllUsesWith(constLhs);
                ++algebraicIdentitiesTimes;
              }
              else if (constRhs && constRhs->isZero()) {
                // x * 0 -> 0
                binOp->replaceAllUsesWith(constRhs);
                ++algebraicIdentitiesTimes;
              }
              break;
            }
            // 4. 除法恒等式
            case Instruction::UDiv: {
              if (constRhs && constRhs->isOne()) {
                // x / 1 -> x
                binOp->replaceAllUsesWith(lhs);
                ++algebraicIdentitiesTimes;
              }
              else if (constLhs && constLhs->isZero()) {
                // 0 / x -> 0
                binOp->replaceAllUsesWith(constLhs);
                ++algebraicIdentitiesTimes;
              }
              break;
            }
            // 5. 取模恒等式
            case Instruction::URem: {
              if (constRhs && constRhs->isOne()) {
                // x % 1 -> 0
                binOp->replaceAllUsesWith(ConstantInt::get(
                  lhs->getType(), 
                  0));
                ++algebraicIdentitiesTimes;
              }
              break;
            }
            default:
              break;
          }
        }
      }
    }
  }

  mOut << "Algebraic Identities: " << algebraicIdentitiesTimes << " times\n";

  return PreservedAnalyses::all();
}