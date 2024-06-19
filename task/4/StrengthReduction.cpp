#include "StrengthReduction.hpp"

using namespace llvm;

PreservedAnalyses
StrengthReduction::run(Module& mod, ModuleAnalysisManager& mam)
{
  int strengthReductionTimes = 0;

  // 函数：判断操作数为2的幂次方
  auto isPowerOfTwo = [](int64_t value) -> bool {
    return value > 0 && (value & (value - 1)) == 0;
  };

  for (auto& func : mod) {
    for (auto& bb : func) {
      std::vector<Instruction*> instToErase;
      for (auto& inst : bb) {
        // 二元运算指令
        if (auto binOp = dyn_cast<BinaryOperator>(&inst)) {
          Value* lhs = binOp->getOperand(0);
          Value* rhs = binOp->getOperand(1);
          auto constLhs = dyn_cast<ConstantInt>(lhs);
          auto constRhs = dyn_cast<ConstantInt>(rhs);

          switch (binOp->getOpcode()) {
            // 1. 2幂乘法 -> 左移
            case Instruction::Mul: {
              if (constLhs || constRhs) {
                if (constLhs && constRhs)
                  mOut << "'constLhs && constRhs' in StrengthReduction\n";

                int64_t constValue = constLhs ? constLhs->getSExtValue() : constRhs->getSExtValue();
                auto operand = constLhs ? rhs : lhs;
                if (isPowerOfTwo(constValue)) {
                  // 乘法转移为移位
                  binOp->replaceAllUsesWith(BinaryOperator::Create(
                    Instruction::Shl, 
                    operand, 
                    ConstantInt::get(
                      operand->getType(), 
                      log2(constValue)),
                    "", 
                    &inst));
                  instToErase.push_back(binOp);
                  ++strengthReductionTimes;
                }
              }
              break;
            }
            // 2. 2幂取模 -> 低位 and mask
            case Instruction::URem: {
              if (constRhs) {
                int64_t constValue = constRhs->getSExtValue();
                if (isPowerOfTwo(constValue)) {
                  // 取模转移为与操作
                  binOp->replaceAllUsesWith(BinaryOperator::Create(
                    Instruction::And, 
                    lhs, 
                    ConstantInt::get(
                      lhs->getType(), 
                      constValue - 1), // 低位为1
                    "", 
                    &inst));
                  instToErase.push_back(binOp);
                  ++strengthReductionTimes;
                }
              }
              break;
            }
            // 3. 2幂除法 -> 右移
            case Instruction::UDiv: {
              if (constRhs) {
                int64_t constValue = constRhs->getSExtValue();
                if (isPowerOfTwo(constValue)) {
                  // 除法转移为移位
                  binOp->replaceAllUsesWith(BinaryOperator::Create(
                    Instruction::LShr, 
                    lhs, 
                    ConstantInt::get(
                      lhs->getType(), 
                      log2(constValue)),
                    "", 
                    &inst));
                  instToErase.push_back(binOp);
                  ++strengthReductionTimes;
                }
              }
            }
            default:
              break;
          }
        }
      }
      for (auto inst : instToErase) {
        inst->eraseFromParent();
      }
    }
  }

  mOut << "StrengthReduction: " << strengthReductionTimes << " times\n";

  return PreservedAnalyses::all();
}