#include <algorithm>
#include <array>
#include <fstream>
#include <iostream>
#include <sstream>
#include <string>
#include <vector>
#include <iomanip>

enum {
  ERR_NO,
  ERR_CANT_READ_FILE,
  ERR_FILE_IS_EMPTY,
  ERR_TOKEN_UNDEFINED,
  ERR_TOKEN_AMBIGIOUS,
};

constexpr int n_keywords = 24;
const std::array<std::string, n_keywords> token_strs{
    "+", "+=", "!",    "!=",     "=",  "==",    "(",     ")",
    "{", "}",  "func", "return", "if", "while", "print", "else",
    "?", ":",  ";",    "&&",     "-",  "*",     "/",     "||",
};
constexpr int n_tokens = n_keywords + 2;
constexpr int ind_token_num = n_keywords + 0;
constexpr int ind_token_id =  n_keywords + 1;
const std::string token_id_stops = "+=!(){}?:;&|-*/";
inline bool is_id(const char c) { 
  return token_id_stops.find(c) == std::string::npos; 
};

struct Token
{
  std::string str;
  int col = 0; // aka line pos index + 1
  int row = 0; // aka line index + 1
  enum {
    PLUS,
    ADDITION_ASSIGNMENT,
    NOT,
    NOT_EQUALS,
    ASSGN,
    EQUALS,
    PARENTHES_OPEN,
    PARENTHES_CLOSE,
    SCOPE_OPEN,
    SCOPE_CLOSE,
    FUNC,
    RETURN,
    IF,
    WHILE,
    PRINT,
    ELSE,
    INPUT,
    COLON,
    SEMICOLON,
    AND,
    MINUS,
    ASTERISK,
    SLASH,
    OR,
    // ^ add new keywords ^
    NUMBER,
    ID,

    ind_size,
  };
  int ind = -1;
  int err = ERR_TOKEN_UNDEFINED;
};

struct TokenStack
{
  virtual ~TokenStack() = default;
  virtual void push_keyword(int col, int row, int keyword) = 0;
  virtual void push_err(int col, int row, int err) = 0;
  virtual void push_num(int col, int row, const std::string &str_num) = 0;
  virtual void push_id(int col, int row, const std::string &str_id) = 0;
};

int tokenize2(std::istream &ss, TokenStack &token_stack,
              int verbose_level = 0) {
  using std::array;
  using std::cout;
  using std::fill;
  using std::getline;
  using std::iswspace;
  using std::max_element;
  using std::string;

  array<int, n_tokens> token_matches;
  array<int, n_tokens> token_matches_next;
  fill(token_matches.begin(), token_matches.end(), 0);

  string s;
  int row = 0;
  while (getline(ss, s)) {
    ++row;
    const int len = s.size();
    if (verbose_level >= 2)
      cout << "next is line " << row << "'" << s << "' (" << len << ")\n";
    /// `i` is pos in str
    /// `t` is pos in token_strs
    bool has_any_token = false;
    bool has_inced_any = false;
    char c;
    for (int i = -1, t = 0; i < len; ++i, ++t) {
      if (i == len - 1)
        goto define_token;
      c = s[i + 1]; // next char
      if (verbose_level >= 2)
        cout << "next is char '" << c << "' at token pos " << t << "\n";
      if (iswspace(c))
        goto define_token;

      has_any_token = true;
      token_matches_next = token_matches;
      // inc tokens if needed
      for (int token_ind = 0; token_ind < n_keywords; ++token_ind) {
        // not nulled already
        if (token_matches_next[token_ind] < 0)
          continue;
        // has enought length
        // has next symbol eq
        if (token_strs[token_ind].size() > t && token_strs[token_ind][t] == c) {
          ++token_matches_next[token_ind];
          if (verbose_level >= 3)
            cout << "  inc " << token_strs[token_ind] << " to "
                 << token_matches_next[token_ind] << "\n";
          continue;
        }
        token_matches_next[token_ind] = -1;
      }
      // inc num if needed
      {
        int &len_match_num = token_matches_next[ind_token_num];
        // can't start from 0
        if (len_match_num >= 0 && isdigit(c)) {
          ++len_match_num;
          if (verbose_level >= 3)
            cout << "  inc number to " << len_match_num << "\n";
        } else
          len_match_num = -1;
      }
      // inc id if needed
      {
        int &len_match_id = token_matches_next[ind_token_id];
        // can't start from digit
        // can't contain any of prohobited symbols
        if (len_match_id >= 0 && is_id(c) && !(len_match_id == 0 && isdigit(c))) {
          ++len_match_id;
          if (verbose_level >= 3)
            cout << "  inc id to " << len_match_id << "\n";
        } else
          len_match_id = -1;
      }
      // if ruined all tokens
      if (-1 == *max_element(token_matches_next.begin(), token_matches_next.end())) {
        --i;
        if (verbose_level >= 3)
            cout << "  moved pos in line " << i << " -> " << (i - 1) << "\n";
        goto define_token;
      }

      token_matches = token_matches_next;
      continue;
    define_token:
      if (!has_any_token) {
        t = -1;
        continue; // skip if prev was space anyway
      }
      if (verbose_level >= 2)
        cout << "whats a token is it?\n";

      const int token_match_best =
          *max_element(token_matches.begin(), token_matches.end());
      int token_ind_best = -1;
      bool is_ambigious = false;
      for (int token_ind = 0; token_ind < n_tokens; ++token_ind) {
        if (token_matches[token_ind] != token_match_best)
          continue;

        // can be keyword
        if (token_strs.size() > token_ind &&
            token_strs[token_ind].size() <= token_match_best) {
          if (verbose_level >= 2)
            cout << "  candidate '" << token_strs[token_ind] << "' with "
                 << token_match_best << " out of "
                 << token_strs[token_ind].size() << "\n";
          const bool was_set = token_ind_best != -1;
          if (was_set) {
            is_ambigious = true;
            break;
          }
          token_ind_best = token_ind;
        }
        // can be id/num
        if (token_ind >= n_keywords) {
          if (verbose_level >= 2) {
            cout << "  candidate ";
            if (token_ind == ind_token_num)
              cout << "number";
            else if (token_ind == ind_token_id)
              cout << "id";
            cout << " of " << token_match_best << "\n";
          }
          if (token_ind_best == -1)
            token_ind_best = token_ind;
        }
      }
      if (verbose_level >= 1)
        cout << "it's a...";
      
      const bool ruined_all = (-1 == *max_element(token_matches_next.begin(), token_matches_next.end()));
      const int token_col = 2 + i + ruined_all - token_match_best;
      const int token_row = row;

      if (token_ind_best == -1) {
        token_stack.push_err(token_row, token_col, ERR_TOKEN_UNDEFINED);
        if (verbose_level >= 1)
          cout << " undefined token!\n";
      } else if (is_ambigious) {
        token_stack.push_err(token_row, token_col, ERR_TOKEN_AMBIGIOUS);
        if (verbose_level >= 1)
          cout << " ambigious call!\n";
      } else if (token_ind_best < ind_token_num) {
        token_stack.push_keyword(token_row, token_col, token_ind_best);
        if (verbose_level >= 1)
          cout << " '" << token_strs[token_ind_best] << "'\n";
      } else {
        const string substr = s.substr(token_col - 1, token_match_best);
        if (token_ind_best == ind_token_num)
          token_stack.push_num(token_row, token_col, substr);
        if (token_ind_best == ind_token_id)
          token_stack.push_id(token_row, token_col, substr);

        if (token_ind_best == ind_token_num && verbose_level >= 1)
          cout << " number " << substr << "\n";
        else if (token_ind_best == ind_token_id && verbose_level >= 1)
          cout << " id " << substr << "\n";
      }

      fill(token_matches.begin(), token_matches.end(), 0);
      has_any_token = false;
      t = -1;
      continue;
      // end define_token
    }
  }
  return ERR_NO;
}

int tokenize_string(std::string const &str, TokenStack &token_stack, int verbose_level = 0) {
  std::stringstream in(str);
  return tokenize2(in, token_stack, verbose_level);
}

struct TokenStackOutStream final : TokenStack
{
  TokenStackOutStream(std::ostream &out, int verbose_level = 0) : 
    out_(out), verbose_level_(verbose_level)
  {}
  inline void push_keyword(int, int, int keyword) override {
    out_ << token_strs[keyword] << "\n";
  }
  inline void push_err(int, int, int err) override {
    if (verbose_level_ > 0)
      out_ << "err code ";
    else
      out_ << "!";
    out_ << err << "\n";
  }
  inline void push_num(int, int, const std::string &str_num) override {
    if (verbose_level_ > 0)
      out_ << "number '"; 
    out_ << str_num;
    if (verbose_level_ > 0)
      out_ << "'";
    out_ << "\n";
  }
  inline void push_id(int, int, const std::string &str_id) override {
    if (verbose_level_ > 0)
      out_ << "number '"; 
    out_ << str_id;
    if (verbose_level_ > 0)
      out_ << "'";
    out_ << "\n";
  }
private:
  std::ostream &out_;
  int verbose_level_ = 0;
};

struct TokenStack2 final : TokenStack
{
  inline void push_keyword(int col, int row, int keyword) override {
    tokens_.push_back(Token{"", col, row, keyword, ERR_NO});
  }
  inline void push_err(int col, int row, int err) override {
    tokens_.push_back(Token{"", col, row, -1, err});
  }
  inline void push_num(int col, int row, const std::string &str_num) override {
    tokens_.push_back(Token{str_num, col, row, Token::NUMBER, ERR_NO});
  }
  inline void push_id(int col, int row, const std::string &str_id) override {
    tokens_.push_back(Token{str_id, col, row, Token::ID, ERR_NO});
  }
  inline bool has_next() const { 
    return pos_ < tokens_.size();
  }
  inline Token pop() { 
    if (!has_next()) {
      throw std::runtime_error("no tokens to pop"); 
      return {}; 
    }
    return tokens_[pos_++];
  }
  // TODO: remove, its for recursive parser
  inline const Token &at(int ind) const {
    return tokens_[ind];
  }
  inline bool has_at(int ind) const {
    return tokens_.size() > ind;    
  }
private:
  std::vector<Token> tokens_;
  int pos_ = 0;
};

std::pair<int, std::string> tokenize_test_in(std::string const &filename,
                                             int verbose_level = 0) {
  std::ifstream in;
  in.open(filename, std::ios::binary);
  if (!in)
    return {ERR_CANT_READ_FILE, ""};
  std::stringstream out;
  TokenStackOutStream tokens_out(out);
  const int code = tokenize2(in, tokens_out, verbose_level);
  in.close();
  return {code, out.str()};
}

std::pair<int, std::string> read_test_out(std::string const &filename) {
  std::ifstream in;
  in.open(filename, std::ios::binary);
  if (!in)
    return {ERR_CANT_READ_FILE, ""};
  std::string s;
  std::stringstream ss;
  int code = ERR_FILE_IS_EMPTY;
  bool is_code_set = false;
  while (std::getline(in, s)) {
    if (!is_code_set) {
      code = std::stoi(s);
      is_code_set = true;
      continue;
    }
    ss << s << "\n";
  }
  in.close();
  return {code, ss.str()};
}

int run_lexer_test(const std::string &num) {
  const bool verbose = true;
  if (verbose)
    std::cout << "Test '" << num << "': ";
  const auto in = "in" + num;
  const auto out = "out" + num;
  const auto in_res = tokenize_test_in(in);
  const auto out_res = read_test_out(out);
  if (in_res == out_res) {
    if (verbose)
      std::cout << "PASS\n";
    return 0;
  }
  if (verbose)
    std::cout << "FAIL!\n";
  std::cerr << "EXPECTED:\n" << out_res.first << "\n" << out_res.second;
  std::cerr << "ACTUAL:\n" << in_res.first << "\n" << in_res.second;
  if (verbose) {
      std::cout << "DEBUG:\n"; 
      tokenize_test_in(in, 10);
  }
  return 1;
}


struct Node
{
  enum {
    TOKEN =      0x000, 
    EXPRESSION = 0x010,
    STATEMENT  = 0x100,
  };
  Node(int ind, int val = TOKEN) : ind_(ind), val_(val) {}
  virtual ~Node() = default;

  int dump_depth = 0;
  static void dump_sep(std::ostream &out, int depth) { out << std::setw(depth * 2) << ""; }  
  virtual void dump(std::ostream &out) { dump_sep(out, dump_depth); }

  // TODO: fix it to observer pattern
  bool is_token(int ind) const { return val_ == TOKEN && ind_ == ind; }
  bool has_type(int val) const { return val_ & val; }
protected:
  int ind_ = -1; /// token ind
  int val_ = TOKEN;
  static const int verbose_level = 1;
};

struct NodeStatement : Node
{
  void dump(std::ostream &out) override {
    Node::dump(out);
    if (verbose_level >= 2)
      std::cout << "stat ";
  }
protected:
  NodeStatement() : Node(-1, STATEMENT) {}
};

struct NodeExpr : Node
{
  void dump(std::ostream &out) override {
    Node::dump(out);
    if (verbose_level >= 2)
      std::cout << "expr ";
  }
protected:
  NodeExpr(int ind) : Node(ind, EXPRESSION) {}
};

// aka 1 statement
struct NodeSemicolon : NodeStatement
{
  NodeSemicolon(NodeExpr *expr) : NodeStatement(), expr(expr) {}
  ~NodeSemicolon() override {
    delete expr;
  }
  void dump(std::ostream &out) override {
    if (verbose_level >= 2) {
      NodeStatement::dump(out);
      std::cout << "NodeSemicolon\n";
      expr->dump_depth = dump_depth + 1;
    } else
      expr->dump_depth = dump_depth;
    expr->dump(out);
  }
  NodeExpr *expr = nullptr;
};

// aka statements list
struct NodeBlock : NodeStatement
{
  NodeBlock(NodeStatement *head, NodeStatement *next) : 
      NodeStatement(), head(head), next(next)
  {}
  ~NodeBlock() override {
    delete head;
    delete next;
  }
  void dump(std::ostream &out) override {
    if (verbose_level >= 2) {
      NodeStatement::dump(out);
      std::cout << "NodeBlock\n";
    }
    head->dump_depth = dump_depth;
    head->dump(out);
    if (next) {
      next->dump_depth = dump_depth;
      next->dump(out);
    }
  }
  NodeStatement *head = nullptr;
  NodeStatement *next = nullptr;
};

struct NodeNum final : NodeExpr
{
  NodeNum(const std::string &str) : NodeExpr(Token::NUMBER), str(str) {}
  void dump(std::ostream &out) override { 
    NodeExpr::dump(out);
    out << str << "\n"; 
  }
  std::string str;
};

struct NodePlus final : NodeExpr
{
  NodePlus(NodeExpr *left, NodeExpr *right) : NodeExpr(Token::PLUS), left(left), right(right) {}
  ~NodePlus() override {
    delete left;
    delete right;
  }
  void dump(std::ostream &out) override {
    NodeExpr::dump(out);
    out << "+\n";
    left->dump_depth = dump_depth + 1;
    right->dump_depth = dump_depth + 1;
    left->dump(out);
    right->dump(out);
  }
  NodeExpr *left = nullptr;
  NodeExpr *right = nullptr;
};

struct NodeParenthes final : NodeExpr
{
  NodeParenthes(NodeExpr *expr) : NodeExpr(Token::PARENTHES_OPEN), expr(expr) {}
  ~NodeParenthes() override {
    delete expr;
  }
  void dump(std::ostream &out) override {
    NodeExpr::dump(out);
    out << "()\n";
    expr->dump_depth = dump_depth + 1;
    expr->dump(out);
  }
  NodeExpr *expr = nullptr;
};

struct NodeScope final : NodeExpr
{
  NodeScope(NodeStatement *stat) : NodeExpr(Token::SCOPE_OPEN), stat(stat) {}
  ~NodeScope() override {
    delete stat;
  }
  void dump(std::ostream &out) override {
    NodeExpr::dump(out);
    out << "{}\n";
    stat->dump_depth = dump_depth + 1;
    stat->dump(out);
  }
  NodeStatement *stat = nullptr;
};

struct NodeNot final : NodeExpr
{
  NodeNot(NodeExpr *expr) : NodeExpr(Token::NOT), expr(expr) {}
  ~NodeNot() override {
    delete expr;
  }
  void dump(std::ostream &out) override {
    NodeExpr::dump(out);
    out << "!\n";
    expr->dump_depth = dump_depth + 1;
    expr->dump(out);
  }
  NodeExpr *expr = nullptr;
};


/*

NodeExpr *parse_expr(TokenStack2 &, int, int &);
NodeExpr *parse_parenthes(TokenStack2 &, int, int &);
NodeExpr *parse_plus(TokenStack2 &, int, int &);
NodeExpr *parse_not(TokenStack2 &, int, int &);
NodeExpr *parse_num(TokenStack2 &, int, int &);

void parse2(TokenStack2 &token_stack) {
  std::cout << "  parse2\n";
  int at = 0;
  int at_new = 0;
  NodeExpr *n = parse_expr(token_stack, at, at_new);
  if (!n) {
    std::cout << "    n is null\n";
    return;
  }
  n->dump(std::cout);
}

bool expect(int type, TokenStack2 &token_stack, int at, int &at_new) {
  if (!token_stack.has_at(at))
    return false;
  const bool ok = (token_stack.at(at).ind == type);
  if (ok) 
    at_new = at + 1;
  std::cout << "    ok = " << ok << ", at = " << at << ", type = " << type << "\n";
  return ok;
}

NodeExpr *parse_expr(TokenStack2 &token_stack, int at, int &at_new) {
  std::cout << "  parse_expr at " << at << "\n";
  int at_new_alt = at;
  NodeExpr *n = nullptr;
  if (false
      || (n = parse_parenthes(token_stack, at, at_new_alt))
      || (n = parse_plus(token_stack, at, at_new_alt))
      || (n = parse_not(token_stack, at, at_new_alt))
      || (n = parse_num(token_stack, at, at_new_alt))
    ) {
    at_new = at_new_alt;
    return n;
  }
  delete n;
  return nullptr;  
}

NodeExpr *parse_num(TokenStack2 &token_stack, int at, int &at_new) {
  std::cout << "  parse_num at " << at << "\n";
  if (!expect(Token::NUMBER, token_stack, at, at))
    return nullptr;

  at_new = at;
  return new NodeNum(token_stack.at(at - 1).str);
}

NodeExpr *parse_plus(TokenStack2 &token_stack, int at, int &at_new) {
  std::cout << "  parse_plus at " << at << "\n";
  NodeExpr *left = nullptr;
  NodeExpr *right = nullptr;
  if (!(left = parse_expr(token_stack, at, at))
      || !expect(Token::PLUS, token_stack, at, at)
      || !(right = parse_expr(token_stack, at, at))) {
    delete left;
    delete right;
    return nullptr;
  }
  at_new = at;
  return new NodePlus(left, right);
}

NodeExpr *parse_parenthes(TokenStack2 &token_stack, int at, int &at_new) {
  std::cout << "  parse_parenthes at " << at << "\n";
  NodeExpr *expr = nullptr;
  if (!expect(Token::PARENTHES_OPEN, token_stack, at, at)
      || !(expr = parse_expr(token_stack, at, at))
      || !expect(Token::PARENTHES_CLOSE, token_stack, at, at)) {
    std::cout << "  parse_parenthes fails " << at << "\n";
    delete expr;
    return nullptr;
  }
  at_new = at;
  return new NodeParenthes(expr);
}

NodeExpr *parse_not(TokenStack2 &token_stack, int at, int &at_new) {
  std::cout << "  parse_not at " << at << "\n";
  NodeExpr *expr = nullptr;
  if (!expect(Token::NOT, token_stack, at, at)
      || !(expr = parse_expr(token_stack, at, at))) {
    delete expr;
    return nullptr;
  }
  at_new = at;
  return new NodeNot(expr);
}
*/




void parse(TokenStack2 &token_stack) {
  std::vector<Node *> nodes;
  const auto node_at = [&nodes](const int ind_neg) -> Node* { 
    return nodes[nodes.size() + ind_neg]; 
  };

  while (token_stack.has_next()) {
    const Token token = token_stack.pop();
    if (token.err != ERR_NO) {
      std::cout << "err in token " << token.err << "\n";
      return;
    }
    if (token.ind != Token::NUMBER) {
      nodes.push_back(new Node(token.ind));
    } else {
      nodes.push_back(new NodeNum(token.str));
    }

    /// check the rules
    for (;;) {
      // blocks
      if (nodes.size() >= 3
          && node_at(-3)->is_token(Token::SCOPE_OPEN)
          && node_at(-2)->has_type(Node::STATEMENT)
          && node_at(-1)->is_token(Token::SCOPE_CLOSE)) {
        Node *node = new NodeScope(
            static_cast<NodeStatement *>(node_at(-2)));
        delete node_at(-3);
        delete node_at(-1);
        nodes.pop_back();
        nodes.pop_back();
        nodes.pop_back();
        nodes.push_back(node);
        continue;
      }
      if (nodes.size() >= 2
          && node_at(-2)->has_type(Node::STATEMENT)
          && node_at(-1)->has_type(Node::STATEMENT)) {
        Node *node = new NodeBlock(
          static_cast<NodeStatement *>(node_at(-2)),
          static_cast<NodeStatement *>(node_at(-1)));        
        nodes.pop_back();
        nodes.pop_back();
        nodes.push_back(node);
        continue;
      }
      if (nodes.size() >= 2
          && node_at(-2)->has_type(Node::EXPRESSION)
          && node_at(-1)->is_token(Token::SEMICOLON)) {
        Node *node = new NodeSemicolon(static_cast<NodeExpr *>(node_at(-2)));
        delete node_at(-1);
        nodes.pop_back();
        nodes.pop_back();
        nodes.push_back(node);
        continue;
      }
      // parens
      if (nodes.size() >= 3
          && node_at(-3)->is_token(Token::PARENTHES_OPEN)
          && node_at(-2)->has_type(Node::EXPRESSION)
          && node_at(-1)->is_token(Token::PARENTHES_CLOSE)) {
        Node *expr = node_at(-2);
        delete node_at(-3);
        delete node_at(-1);
        nodes.pop_back();
        nodes.pop_back();
        nodes.pop_back();
        nodes.push_back(expr);
        continue;
      }
      // binary ops
      if (nodes.size() >= 3
          && node_at(-3)->has_type(Node::EXPRESSION)
          && node_at(-2)->is_token(Token::PLUS)
          && node_at(-1)->has_type(Node::EXPRESSION)) {
        Node *plus = new NodePlus(
            static_cast<NodeExpr *>(node_at(-3)), 
            static_cast<NodeExpr *>(node_at(-1)));
        delete node_at(-2);
        nodes.pop_back();
        nodes.pop_back();
        nodes.pop_back();
        nodes.push_back(plus);
        continue;
      }
      // unary ops
      if (nodes.size() >= 2
          && node_at(-2)->is_token(Token::NOT)
          && node_at(-1)->has_type(Node::EXPRESSION)) {
        Node *node = new NodeNot(static_cast<NodeExpr *>(node_at(-1)));
        delete node_at(-2);
        nodes.pop_back();
        nodes.pop_back();
        nodes.push_back(node);
        continue;
      }
      break;
    }
  }
  for (Node *node : nodes) {
    if (nodes.size() > 1)
      std::cout << "NODE:\n";
    node->dump(std::cout);
    delete node;
  }
}

int main() {
  run_lexer_test("001");
  run_lexer_test("002");
  run_lexer_test("003");
  run_lexer_test("004");
  run_lexer_test("005");
  run_lexer_test("006");
  run_lexer_test("007");
  run_lexer_test("008");
  run_lexer_test("009");
  run_lexer_test("010"); // nums complex
  run_lexer_test("011"); // nums w/ spaces
  run_lexer_test("012"); // nums w/o spaces
  run_lexer_test("013"); // ids
  run_lexer_test("014"); // ids vs keywords
  run_lexer_test("015"); // ids and nums w/o space
  run_lexer_test("016"); // nums w/ zeros
 
  TokenStackOutStream out(std::cout, 10);
  tokenize_string("1+002+3", out, 0);

  {
    std::cout << "EX1:\n";
    TokenStack2 stack;
    tokenize_string("1+002+3", stack, 0);
    parse(stack);
  }
  {
    std::cout << "EX2:\n";
    TokenStack2 stack;
    tokenize_string("1+(002+3)", stack, 0);
    parse(stack);
  }
  {
    std::cout << "EX3:\n";
    TokenStack2 stack;
    tokenize_string("!1+!(002+!!!3)", stack, 0);
    parse(stack);
  }
  {
    std::cout << "EX4:\n";
    TokenStack2 stack;
    tokenize_string("1+2;3+4;", stack, 0);
    parse(stack);
  }
  {
    std::cout << "EX5:\n";
    TokenStack2 stack;
    tokenize_string("{1+2;3+4;5+6;}", stack, 0);
    parse(stack);
  }

  return 0;
}
