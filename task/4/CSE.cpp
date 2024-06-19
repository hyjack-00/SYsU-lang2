#include "CSE.hpp"

using namespace llvm;

PreservedAnalyses
CSE::run(Module& mod, ModuleAnalysisManager& mam)
{


  return PreservedAnalyses::all();
}