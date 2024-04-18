#include "SYsULexer.h" // 确保这里的头文件名与您生成的词法分析器匹配
#include <fstream>
#include <iostream>
#include <unordered_map>

// // 映射定义，将ANTLR的tokenTypeName映射到clang的格式
// std::unordered_map<std::string, std::string> tokenTypeMapping = {
//   { "Int", "int" },
//   { "Identifier", "identifier" },
//   { "LeftParen", "l_paren" },
//   { "RightParen", "r_paren" },
//   { "RightBrace", "r_brace" },
//   { "LeftBrace", "l_brace" },
//   { "LeftBracket", "l_square" },
//   { "RightBracket", "r_square" },
//   { "Constant", "numeric_constant" },
//   { "Return", "return" },
//   { "Semi", "semi" },
//   { "EOF", "eof" },
//   { "Equal", "equal" },
//   { "Plus", "plus" },
//   { "Comma", "comma" },
//   // 在这里继续添加其他映射
// };

std::string toLowerString(const std::string& str) {
    std::string result = str;
    for (char& c : result) {
        c = std::tolower(c);
    }
    return result;
}

void
print_token(const antlr4::Token* token,
            const antlr4::CommonTokenStream& tokens,
            std::ofstream& outFile,
            const antlr4::Lexer& lexer)
{
  auto& vocabulary = lexer.getVocabulary();

  auto tokenTypeName =
    std::string(vocabulary.getSymbolicName(token->getType()));

  if (tokenTypeName.empty())
    tokenTypeName = "<UNKNOWN>"; // 处理可能的空字符串情况

  static std::string fileName;
  static int lines = 0;
  static bool startOfLine = true;
  static bool leadingSpace = false;

  if (tokenTypeName == "LineAfterPreprocessing") {
    // outFile << "DEBUG LineAfterPreprocessing " << std::endl;
    /*
      # 1 "<command line>" 1 2
        ^ ^              ^
        2 p1             p2  
    */
    std::string preprocessLine = token->getText();
    uint p1 = preprocessLine.find_first_of('"');
    uint p2 = preprocessLine.find_first_of('"', p1 + 1);
    lines = std::stoi(preprocessLine.substr(2, p1 - 3)) - 1;
    fileName = preprocessLine.substr(p1 + 1, p2 - p1 - 1);
    return;
  }
  else if (tokenTypeName == "Whitespace") {
    // outFile << "DEBUG Whitespace" << std::endl;
    leadingSpace = true;
    return;
  }
  else if (tokenTypeName ==  "Newline") {
    // outFile << "DEBUG Newline" << std::endl;
    startOfLine = true;
    lines ++;
    return;
  }

  // 大小写转换即可
  // if (tokenTypeMapping.find(tokenTypeName) != tokenTypeMapping.end()) {
  //   tokenTypeName = tokenTypeMapping[tokenTypeName];
  // }
  tokenTypeName = toLowerString(tokenTypeName);
  int offsets = token->getCharPositionInLine() + 1;

  if (token->getText() != "<EOF>") {
    outFile << tokenTypeName << " '" << token->getText() << "'";
  }
  else {
    outFile << tokenTypeName << " ''";
    startOfLine = false;
  }
  
  outFile << "\t";
  if (startOfLine) {
    outFile << " [StartOfLine]";
    startOfLine = false;
  }
  if (leadingSpace) {
    outFile << " [LeadingSpace]";
    leadingSpace = false;
  }
  outFile << "\t";

  outFile << "Loc=<" 
    << fileName << ":"
    << lines << ":" 
    << offsets << ">" 
    << std::endl;
}

int
main(int argc, char* argv[])
{
  if (argc != 3) {
    std::cout << "Usage: " << argv[0] << " <input> <output>\n";
    return -1;
  }

  std::ifstream inFile(argv[1]);
  if (!inFile) {
    std::cout << "Error: unable to open input file: " << argv[1] << '\n';
    return -2;
  }

  std::ofstream outFile(argv[2]);
  if (!outFile) {
    std::cout << "Error: unable to open output file: " << argv[2] << '\n';
    return -3;
  }

  std::cout << "程序 '" << argv[0] << std::endl;
  std::cout << "输入 '" << argv[1] << std::endl;
  std::cout << "输出 '" << argv[2] << std::endl;

  antlr4::ANTLRInputStream input(inFile);
  SYsULexer lexer(&input);

  antlr4::CommonTokenStream tokens(&lexer);
  tokens.fill();

  for (auto&& token : tokens.getTokens()) {
    print_token(token, tokens, outFile, lexer);
  }
}
