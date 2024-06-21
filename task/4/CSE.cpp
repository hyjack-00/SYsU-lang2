#include "CSE.hpp"

using namespace llvm;

/*
利用类似哈希表的形式表示 func 内的历史指令：
二元运算符、操作数1、操作数2
一元运算符、操作数、空

暂时不考虑其他指令：load, store, jump ...

若当前指令与历史指令相同，则直接替换为历史指令的结果，删除当前指令
*/

PreservedAnalyses
CSE::run(Module& mod, ModuleAnalysisManager& mam)
{
  int cseTimes = 0;

  for (auto& func : mod) {
    std::unordered_map<std::string, Value*> exprToValue;

    for (auto& bb : func) {
      std::vector<Instruction*> instToErase;
      for (auto& inst : bb) {
        // 二元运算指令
        if (auto binOp = dyn_cast<BinaryOperator>(&inst)) {
          std::string expr;
          

          if (exprToValue.find(expr) != exprToValue.end()) {
            binOp->replaceAllUsesWith(exprToValue[expr]);
            instToErase.push_back(binOp);
            ++cseTimes;
          } else {
            exprToValue[expr] = binOp;
          }
        }
      }

      for (auto inst : instToErase) {
        inst->eraseFromParent();
      }
    }
  }

  return PreservedAnalyses::all();
}