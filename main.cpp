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

// TODO: remove this int
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

// TODO: remove this int
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
  const auto in =  "lexer_tests/in" + num;
  const auto out = "lexer_tests/out" + num;
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

struct INodeVisitor;

struct Node
{
  enum {
    TOKEN =      0,
    EXPRESSION = 1 << 0,
    PARENTHES =  1 << 1 | EXPRESSION,
    SCOPE =      1 << 2 | EXPRESSION,
    STATEMENT  = 1 << 3,
  };
  Node(int ind, int val = TOKEN) : ind_(ind), val_(val) {}
  virtual ~Node() = default;
  virtual void accept(INodeVisitor &) const {}

  bool is_token(int ind) const { return val_ == TOKEN && ind_ == ind; }
  bool has_type(int val) const { return val_ & val; }
  int ind_ = -1; /// token ind
protected:
  int val_ = TOKEN;
};


struct NodeBinOp;
struct NodeBlock;
struct NodeSemicolon;
struct NodeNot;
struct NodeNum;
struct NodeScope;
struct NodeParenthes;
struct NodeWhile;

struct INodeVisitor
{
  virtual ~INodeVisitor() = default;
  virtual void visit(NodeBinOp const&) = 0;
  virtual void visit(NodeBlock const&) = 0;
  virtual void visit(NodeSemicolon const&) = 0;
  virtual void visit(NodeNot const&) = 0;
  virtual void visit(NodeNum const&) = 0;
  virtual void visit(NodeScope const&) = 0;
  virtual void visit(NodeParenthes const&) = 0;
  virtual void visit(NodeWhile const&) = 0;
};

struct NodeStatement : Node
{
protected:
  NodeStatement() : Node(-1, STATEMENT) {}
};

struct NodeExpr : Node
{
protected:
  NodeExpr(int ind, int val = EXPRESSION) : Node(ind, val) {}
};


// aka 1 statement
struct NodeSemicolon : NodeStatement
{
  NodeSemicolon(NodeExpr *expr) : NodeStatement(), expr(expr) {}
  ~NodeSemicolon() override {
    delete expr;
  }
  void accept(INodeVisitor &v) const override {
    v.visit(*this);
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
  void accept(INodeVisitor &v) const override {
    v.visit(*this);
  }
  NodeStatement *head = nullptr;
  NodeStatement *next = nullptr;
};

struct NodeNum final : NodeExpr
{
  NodeNum(const std::string &str) : NodeExpr(Token::NUMBER), str(str) {}
  void accept(INodeVisitor &v) const override {
    v.visit(*this);
  }
  std::string str;
};

struct NodeBinOp : NodeExpr
{
  NodeBinOp(int ind, int priority, NodeExpr *left, NodeExpr *right) :
    NodeExpr(ind), priority(priority), left(left), right(right)
  {}
  ~NodeBinOp() override {
    delete left;
    delete right;
  }
  void accept(INodeVisitor &v) const override {
    v.visit(*this);
  }
  int priority = 0;
  NodeExpr *left = nullptr;
  NodeExpr *right = nullptr;
};

enum {
  PRIORITY_PLUS  = 1,
  PRIORITY_MINUS = PRIORITY_PLUS,
  PRIORITY_MULT  = 2,
};

struct NodePlus final : NodeBinOp
{
  NodePlus(NodeExpr *left, NodeExpr *right) :
    NodeBinOp(Token::PLUS, PRIORITY_PLUS, left, right)
  {}
};

struct NodeMinus final : NodeBinOp
{
  NodeMinus(NodeExpr *left, NodeExpr *right) :
    NodeBinOp(Token::MINUS, PRIORITY_MINUS, left, right)
  {}
};

struct NodeMult final : NodeBinOp
{
  NodeMult(NodeExpr *left, NodeExpr *right) :
    NodeBinOp(Token::ASTERISK, PRIORITY_MULT, left, right)
  {}
};

struct NodeParenthes final : NodeExpr
{
  NodeParenthes(NodeExpr *expr) : NodeExpr(-1, Node::PARENTHES), expr(expr) {}
  ~NodeParenthes() override {
    delete expr;
  }
  void accept(INodeVisitor &v) const override {
    v.visit(*this);
  }
  NodeExpr *expr = nullptr;
};

struct NodeScope final : NodeExpr
{
  NodeScope(NodeStatement *stat) : NodeExpr(-1, Node::SCOPE), stat(stat) {}
  ~NodeScope() override {
    delete stat;
  }
  void accept(INodeVisitor &v) const override {
    v.visit(*this);
  }
  NodeStatement *stat = nullptr;
};

struct NodeWhile : NodeStatement
{
  NodeWhile(NodeParenthes *cond, NodeScope *body) :
    NodeStatement(), cond(cond), body(body)
  {}
  ~NodeWhile() override {
    delete cond;
    delete body;
  }
  void accept(INodeVisitor &v) const override {
    v.visit(*this);
  }
  NodeParenthes *cond = nullptr;
  NodeScope *body = nullptr;
};

struct NodeNot final : NodeExpr
{
  NodeNot(NodeExpr *expr) : NodeExpr(Token::NOT), expr(expr) {}
  ~NodeNot() override {
    delete expr;
  }
  void accept(INodeVisitor &v) const override {
    v.visit(*this);
  }
  NodeExpr *expr = nullptr;
};

struct NodeVisitorOut : INodeVisitor
{
  NodeVisitorOut(std::ostream &out, bool is_debug_verbose = false) :
    out(out), is_debug_verbose(is_debug_verbose)
  {}
  std::ostream &out;
  bool is_debug_verbose;
  int offset_ = 0;

  void inc_offset() {++offset_;}
  void dec_offset() {--offset_;}
  void dump_offset() const { out << std::setw(offset_ * 2) << ""; }

  void visit(NodeBinOp const& n) override {
    dump_offset();
    inc_offset();
    out << token_strs[n.ind_] << "\n";
    n.left->accept(*this);
    n.right->accept(*this);
    dec_offset();
  }
  void visit(NodeBlock const& n) override {
    if (is_debug_verbose) {
      dump_offset();
      out << "statements block\n";
    }
    n.head->accept(*this);
    if (n.next)
      n.next->accept(*this);
  }
  void visit(NodeSemicolon const& n) override {
    if (is_debug_verbose) {
      dump_offset();
      out << "statement\n";
    }
    n.expr->accept(*this);
  }
  void visit(NodeNot const& n) override {
    dump_offset();
    inc_offset();
    out << "!\n";
    n.expr->accept(*this);
    dec_offset();
  }
  void visit(NodeNum const& n) override {
    dump_offset();
    inc_offset();
    out << n.str << "\n";
    dec_offset();
  }
  void visit(NodeScope const& n) override {
    dump_offset();
    inc_offset();
    out << "{}\n";
    n.stat->accept(*this);
    dec_offset();
  }
  void visit(NodeParenthes const& n) override {
    dump_offset();
    inc_offset();
    out << "()\n";
    n.expr->accept(*this);
    dec_offset();
  }
  void visit(NodeWhile const& n) override {
    dump_offset();
    inc_offset();
    out << "while\n";
    n.cond->accept(*this);
    n.body->accept(*this);
    dec_offset();
  }
};

void parse(TokenStack2 &token_stack, std::ostream &out) {
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
      // while (expr) {statement}
      if (nodes.size() >= 3
          && node_at(-3)->is_token(Token::WHILE)
          && node_at(-2)->has_type(Node::PARENTHES)
          && node_at(-1)->has_type(Node::SCOPE)) {
        Node *node = new NodeWhile(
              static_cast<NodeParenthes *>(node_at(-2)),
              static_cast<NodeScope *>(node_at(-1)));
        delete node_at(-3);
        nodes.pop_back();
        nodes.pop_back();
        nodes.pop_back();
        nodes.push_back(node);
        continue;
      }
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
        Node *node = new NodeParenthes(
              static_cast<NodeExpr *>(node_at(-2)));
        delete node_at(-3);
        delete node_at(-1);
        nodes.pop_back();
        nodes.pop_back();
        nodes.pop_back();
        nodes.push_back(node);
        continue;
      }
      // binary ops
      const auto is_bin_op_with = [&](const int token_ind) {
        return nodes.size() >= 3
            && node_at(-3)->has_type(Node::EXPRESSION)
            && node_at(-2)->is_token(token_ind)
            && node_at(-1)->has_type(Node::EXPRESSION);
      };
      const auto set_bin_op_left_by_priority = [&](NodeBinOp *binop) {
        NodeBinOp *parent_new = nullptr;
        auto *left_expr = static_cast<NodeExpr *>(node_at(-3));
        for (;;) {
          auto *left_expr_as_binop = dynamic_cast<NodeBinOp *>(left_expr);
          if (!left_expr_as_binop)
            break;
          if (left_expr_as_binop->priority >= binop->priority)
            break;
          parent_new = left_expr_as_binop;
          left_expr = left_expr_as_binop->right;
        }
        binop->left = left_expr;
        delete node_at(-2);
        if (parent_new == nullptr) {
          nodes.pop_back();
          nodes.pop_back();
          nodes.pop_back();
          nodes.push_back(binop);
        } else {
          nodes.pop_back();
          nodes.pop_back();
          parent_new->right = binop;
        }
      };

      if (is_bin_op_with(Token::ASTERISK)) {
        auto *binop = new NodeMult(nullptr, static_cast<NodeExpr *>(node_at(-1)));
        set_bin_op_left_by_priority(binop);
        continue;
      }
      if (is_bin_op_with(Token::PLUS)) {
        auto *binop = new NodePlus(nullptr, static_cast<NodeExpr *>(node_at(-1)));
        set_bin_op_left_by_priority(binop);
        continue;
      }
      if (is_bin_op_with(Token::MINUS)) {
        auto *binop = new NodeMinus(nullptr, static_cast<NodeExpr *>(node_at(-1)));
        set_bin_op_left_by_priority(binop);
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
      out << "NODE:\n";
    NodeVisitorOut visitor(out);
    node->accept(visitor);
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

  
  const auto run_parser_test = [](const std::string &num) {
    const auto file_in =  "parser_tests/in" + num;
    const auto file_out = "parser_tests/out" + num;
    std::cout << "Test '" << num << "': ";
    std::ifstream in;
    in.open(file_in, std::ios::binary);
    if (!in) {
      std::cout << "in file missing\n";
      return;
    }
    TokenStack2 stack;
    const int code = tokenize2(in, stack, 0);
    if (code) {
      std::cout << "can't tokenize\n";
      return;
    }
    in.close();
    std::stringstream out;
    parse(stack, out);
    std::string actual = out.str();
    std::string expect;
    
    // read out
    std::ifstream in2;
    in2.open(file_out, std::ios::binary);
    if (!in2) {
      std::cout << "out file missing\n";
      return;
    }
    std::string s;
    std::stringstream ss;
    while (std::getline(in2, s)) {
      ss << s << "\n";
    }
    in2.close();
    expect = ss.str();
    
    // compare
    if (actual == expect) {
      std::cout << "PASS\n";
      return;
    }
    std::cout << "FAILED\n";
    std::cout << "ACTUAL:\n" << actual << "EXPECTED:\n" << expect;
    return;
  };
  run_parser_test("001");
  run_parser_test("002");
  run_parser_test("003");
  run_parser_test("004");
  run_parser_test("005");
  run_parser_test("006");
  run_parser_test("007");
  run_parser_test("008");
  run_parser_test("008");
  run_parser_test("009");
  run_parser_test("010");

  {
    std::cout << "SAMPLE:\n";
    TokenStack2 stack;
    tokenize_string("1+2+3", stack);
    parse(stack, std::cout);
  }

  return 0;
}
