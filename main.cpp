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

constexpr int n_keywords = 25;
const std::array<std::string, n_keywords> token_strs{
  "+", "+=", "!",    "!=",     "=",  "==",    "(",     ")",
  "{", "}",  "func", "return", "if", "while", "print", "else",
  "?", ":",  ";",    "&&",     "-",  "*",     "/", "||", "//",
};
constexpr int n_tokens = n_keywords + 2;
const std::string token_id_stops = "+=!(){}?:;&|-*/";
inline bool isvarname(const char c) {
  return token_id_stops.find(c) == std::string::npos;
}

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
    ASSIGN,
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
    SCAN,
    COLON,
    SEMICOLON,
    AND,
    MINUS,
    MULT,
    SLASH,
    OR,
    // ^ add new keywords ^

    COMMENT,
    NUMBER,
    ID,

    LAST_KEYWORD_ = COMMENT - 1,
  };
  int ind = -1;
  int err = ERR_TOKEN_UNDEFINED;
};

struct ITokenStack
{
  virtual ~ITokenStack() = default;
  virtual void push_keyword(int col, int row, int keyword) = 0;
  virtual void push_err(int col, int row, int err) = 0;
  virtual void push_num(int col, int row, const std::string &str_num) = 0;
  virtual void push_id(int col, int row, const std::string &str_id) = 0;
};

int tokenize2(std::istream &ss, ITokenStack &token_stack,
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
        int &len_match_num = token_matches_next[Token::NUMBER];
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
        int &len_match_id = token_matches_next[Token::ID];
        // can't start from digit
        // can't contain any of prohobited symbols
        if (len_match_id >= 0 && isvarname(c) && !(len_match_id == 0 && isdigit(c))) {
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
            if (token_ind == Token::NUMBER)
              cout << "number";
            else if (token_ind == Token::ID)
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
      bool is_skipping_til_line_end = false;

      if (token_ind_best == -1) {
        token_stack.push_err(token_row, token_col, ERR_TOKEN_UNDEFINED);
        if (verbose_level >= 1)
          cout << " undefined token!\n";
      } else if (is_ambigious) {
        token_stack.push_err(token_row, token_col, ERR_TOKEN_AMBIGIOUS);
        if (verbose_level >= 1)
          cout << " ambigious call!\n";
      } else if (token_ind_best <= Token::LAST_KEYWORD_) {
        token_stack.push_keyword(token_row, token_col, token_ind_best);
        if (verbose_level >= 1)
          cout << " '" << token_strs[token_ind_best] << "'\n";
      } else if (token_ind_best == Token::COMMENT) { 
        if (verbose_level >= 1)
          cout << " comment\n";
        is_skipping_til_line_end = true;
      } else {
        const string substr = s.substr(token_col - 1, token_match_best);
        if (token_ind_best == Token::NUMBER)
          token_stack.push_num(token_row, token_col, substr);
        if (token_ind_best == Token::ID)
          token_stack.push_id(token_row, token_col, substr);

        if (token_ind_best == Token::NUMBER && verbose_level >= 1)
          cout << " number " << substr << "\n";
        else if (token_ind_best == Token::ID && verbose_level >= 1)
          cout << " id " << substr << "\n";
      }

      fill(token_matches.begin(), token_matches.end(), 0);
      has_any_token = false;
      t = -1;

      if (is_skipping_til_line_end)
        break;
      continue;
      // end define_token
    }
  }
  return ERR_NO;
}

int tokenize_string(std::string const &str, ITokenStack &token_stack, int verbose_level = 0) {
  std::stringstream in(str);
  return tokenize2(in, token_stack, verbose_level);
}

struct TokenStackOutStream final : ITokenStack
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

struct TokenStack final : ITokenStack
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

struct Node;
struct NodeBinOp;
struct NodeBlock;
struct NodeSemicolon;
struct NodeUnOp;
struct NodeNum;
struct NodeScan;
struct NodeId;
struct NodeScope;
struct NodeParenthes;
struct NodeWhile;

struct INodeVisitor
{
  virtual ~INodeVisitor() = default;
  virtual void visit(NodeBinOp const&) = 0;
  virtual void visit(NodeUnOp const&) = 0;
  virtual void visit(NodeBlock const&) = 0;
  virtual void visit(NodeSemicolon const&) = 0;
  virtual void visit(NodeScan const&) = 0;
  virtual void visit(NodeId const&) = 0;
  virtual void visit(NodeNum const&) = 0;
  virtual void visit(NodeScope const&) = 0;
  virtual void visit(NodeParenthes const&) = 0;
  virtual void visit(NodeWhile const&) = 0;
  virtual void visit(Node const&) = 0;
};

struct Node
{
  enum {
    TOKEN =      0,
    EXPRESSION = 1 << 0,
    PARENTHES =  1 << 1 | EXPRESSION,
    SCOPE_ONLY = 1 << 2,
    SCOPE =      1 << 2 | EXPRESSION,
    STATEMENT  = 1 << 3,
  };
  Node(int ind, int val = TOKEN) : ind_(ind), val_(val) {}
  virtual ~Node() = default;
  virtual void accept(INodeVisitor &v) const {
    v.visit(*this);
  }

  bool is_token(int ind) const { return val_ == TOKEN && ind_ == ind; }
  bool has_type(int val) const { return val_ & val; }
  int ind_ = -1; /// token ind
protected:
  int val_ = TOKEN;
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

struct NodeId final : NodeExpr
{
  NodeId(const std::string &str) : NodeExpr(Token::ID), str(str) {}
  void accept(INodeVisitor &v) const override {
    v.visit(*this);
  }
  std::string str;
};

struct NodeScan final : NodeExpr
{
  NodeScan() : NodeExpr(Token::SCAN) {}
  void accept(INodeVisitor &v) const override {
    v.visit(*this);
  }
};

struct NodeOp : NodeExpr
{
  NodeOp(int ind, int priority) : NodeExpr(ind, EXPRESSION), priority(priority){}
  virtual NodeExpr *&last_arg() = 0;
  int priority = 0;
};

struct NodeBinOp : NodeOp
{
  NodeBinOp(int ind, int priority, NodeExpr *left, NodeExpr *right) :
    NodeOp(ind, priority), left(left), right(right)
  {}
  ~NodeBinOp() override {
    delete left;
    delete right;
  }
  void accept(INodeVisitor &v) const override {
    v.visit(*this);
  }
  NodeExpr *&last_arg() override {
    return right;
  }
  NodeExpr *left = nullptr;
  NodeExpr *right = nullptr;
};

struct NodeUnOp : NodeOp
{
  NodeUnOp(int ind, int priority, NodeExpr *expr) :
    NodeOp(ind, priority), expr(expr)
  {}
  ~NodeUnOp() override {
    delete expr;
  }
  void accept(INodeVisitor &v) const override {
    v.visit(*this);
  }
  NodeExpr *&last_arg() override {
    return expr;
  }
  NodeExpr *expr = nullptr;
};

enum {
  PRIORITY_PRINT,
  PRIORITY_ASSIGN,
  PRIORITY_PLUS,
  PRIORITY_MINUS = PRIORITY_PLUS,
  PRIORITY_MULT,
  PRIORITY_NOT,
};

struct NodeAssign final : NodeBinOp
{
  NodeAssign(NodeExpr *left, NodeExpr *right) :
    NodeBinOp(Token::ASSIGN, PRIORITY_ASSIGN, left, right)
  {}
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
    NodeBinOp(Token::MULT, PRIORITY_MULT, left, right)
  {}
};

struct NodeNot final : NodeUnOp
{
  NodeNot(NodeExpr *expr) : NodeUnOp(Token::NOT, PRIORITY_NOT, expr) {}
};

struct NodePrint final : NodeUnOp
{
  NodePrint(NodeExpr *expr) : NodeUnOp(Token::PRINT, PRIORITY_PRINT, expr) {}
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
  NodeWhile(NodeParenthes *cond, Node *body) :
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
  Node *body = nullptr;
};

// struct NodeIf : NodeStatement
// {
//   NodeIf(NodeParenthes *cond, NodeStatement *body) :
//     NodeStatement(), cond(cond), body(body)
//   {}
//   ~NodeIf() override {
//     delete cond;
//     delete body;
//   }
//   void accept(INodeVisitor &v) const override {
//     v.visit(*this);
//   }
//   NodeParenthes *cond = nullptr;
//   NodeScope *body = nullptr;
// };

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
    if (is_debug_verbose)
      out << "binop ";
    out << token_strs[n.ind_] << "\n";
    n.left->accept(*this);
    n.right->accept(*this);
    dec_offset();
  }
  void visit(NodeUnOp const& n) override {
    dump_offset();
    inc_offset();
    if (is_debug_verbose)
      out << "unop ";
    out << token_strs[n.ind_] << "\n";
    n.expr->accept(*this);
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
  void visit(NodeNum const& n) override {
    dump_offset();
    inc_offset();
    if (is_debug_verbose)
      out << "number ";
    out << n.str << "\n";
    dec_offset();
  }
  void visit(NodeId const& n) override {
    dump_offset();
    inc_offset();
    if (is_debug_verbose)
      out << "varname ";
    out << n.str << "\n";
    dec_offset();
  }
  void visit(NodeScan const& n) override {
    dump_offset();
    inc_offset();
    out << "?\n";
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
  void visit(Node const& n) override {
    dump_offset();
    out << "token '"<< token_strs[n.ind_] << "'\n";
  }
};

void parse(TokenStack &token_stack, INodeVisitor &visitor) {
  std::vector<Node *> nodes;
  const auto node_at = [&nodes](const int ind_neg) -> Node* {
    return nodes[nodes.size() + ind_neg];
  };
  const auto pop_x_and_push = [&nodes](int x, Node *node) {
    while (x--)
      nodes.pop_back();
    nodes.push_back(node);
  };

  while (token_stack.has_next()) {
    const Token token = token_stack.pop();
    if (token.err != ERR_NO) {
      std::cout << "err in token " << token.err << "\n";
      return;
    }
    if (token.ind == Token::NUMBER) {
      nodes.push_back(new NodeNum(token.str));
    } else if (token.ind == Token::ID) {
      nodes.push_back(new NodeId(token.str));
    } else if (token.ind == Token::SCAN) {
      nodes.push_back(new NodeScan());
    } else {
      nodes.push_back(new Node(token.ind));
    }

    /// check the rules
    for (;;) {
      // while (expr) {statement}
      if (nodes.size() >= 3
          && node_at(-3)->is_token(Token::WHILE)
          && node_at(-2)->has_type(Node::PARENTHES)
          && node_at(-1)->has_type(Node::SCOPE_ONLY | Node::STATEMENT)) {
        Node *node = new NodeWhile(
              static_cast<NodeParenthes *>(node_at(-2)),
              node_at(-1));
        delete node_at(-3);
        pop_x_and_push(3, node);
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
        pop_x_and_push(3, node);
        continue;
      }
      if (nodes.size() >= 2
          && node_at(-2)->has_type(Node::STATEMENT)
          && node_at(-1)->has_type(Node::STATEMENT)) {
        Node *node = new NodeBlock(
              static_cast<NodeStatement *>(node_at(-2)),
              static_cast<NodeStatement *>(node_at(-1)));
        pop_x_and_push(2, node);
        continue;
      }
      if (nodes.size() >= 2
          && node_at(-2)->has_type(Node::EXPRESSION)
          && node_at(-1)->is_token(Token::SEMICOLON)) {
        Node *node = new NodeSemicolon(static_cast<NodeExpr *>(node_at(-2)));
        delete node_at(-1);
        pop_x_and_push(2, node);
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
        pop_x_and_push(3, node);
        continue;
      }
      // binary ops
      const auto is_bin_op_with = [&](const int token_ind) {
        return nodes.size() >= 3
            && node_at(-3)->has_type(Node::EXPRESSION)
            && node_at(-2)->is_token(token_ind)
            && node_at(-1)->has_type(Node::EXPRESSION | Node::STATEMENT);
      };
      /// right_based is something, which destincts x=y=5 from 1+2-3
      enum class op_side { right, left };
      const auto set_bin_op_left_by_priority = [&](NodeBinOp *binop, op_side side) {
        NodeOp *parent_new = nullptr;
        auto *left_expr = static_cast<NodeExpr *>(node_at(-3));
        for (;;) {
          auto *left_expr_as_op = dynamic_cast<NodeOp *>(left_expr);
          if (!left_expr_as_op)
            break;
          if (left_expr_as_op->priority > binop->priority - (side == op_side::right))
            break;
          parent_new = left_expr_as_op;
          left_expr = left_expr_as_op->last_arg();
        }
        binop->left = left_expr;
        delete node_at(-2);
        if (parent_new == nullptr) {
          pop_x_and_push(3, binop);
        } else {
          nodes.pop_back();
          nodes.pop_back();
          parent_new->last_arg() = binop;
        }
      };
      const auto is_un_op_with = [&](const int token_ind) {
        return nodes.size() >= 2
            && node_at(-2)->is_token(token_ind)
            && node_at(-1)->has_type(Node::EXPRESSION);
      };
      const auto set_un_op_left_by_priority = [&](NodeUnOp *unop) {
        delete node_at(-2);
        pop_x_and_push(2, unop);
      };

      if (is_bin_op_with(Token::ASSIGN)) {
        auto *binop = new NodeAssign(nullptr, static_cast<NodeExpr *>(node_at(-1)));
        set_bin_op_left_by_priority(binop, op_side::left);
        continue;
      }
      if (is_bin_op_with(Token::MULT)) {
        auto *binop = new NodeMult(nullptr, static_cast<NodeExpr *>(node_at(-1)));
        set_bin_op_left_by_priority(binop, op_side::right);
        continue;
      }
      if (is_bin_op_with(Token::PLUS)) {
        auto *binop = new NodePlus(nullptr, static_cast<NodeExpr *>(node_at(-1)));
        set_bin_op_left_by_priority(binop, op_side::right);
        continue;
      }
      if (is_bin_op_with(Token::MINUS)) {
        auto *binop = new NodeMinus(nullptr, static_cast<NodeExpr *>(node_at(-1)));
        set_bin_op_left_by_priority(binop, op_side::right);
        continue;
      }
      // unary ops
      if (is_un_op_with(Token::NOT)) {
        auto *unop = new NodeNot(static_cast<NodeExpr *>(node_at(-1)));
        set_un_op_left_by_priority(unop);
        continue;
      }
      if (is_un_op_with(Token::PRINT)) {
        auto *unop = new NodePrint(static_cast<NodeExpr *>(node_at(-1)));
        set_un_op_left_by_priority(unop);
        continue;
      }
      break;
    }
  }
  for (Node *node : nodes) {
    if (nodes.size() > 1)
      std::cout << "NODE:\n";
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
  run_lexer_test("017"); // comments
  run_lexer_test("018");
  
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
    TokenStack stack;
    const int code = tokenize2(in, stack, 0);
    if (code) {
      std::cout << "can't tokenize\n";
      return;
    }
    in.close();
    std::stringstream out;
    NodeVisitorOut visitor(out);
    parse(stack, visitor);
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
  run_parser_test("011");
  run_parser_test("012");
  run_parser_test("013");
  run_parser_test("014");
  run_parser_test("015"); // while with no {}, just statement
  run_parser_test("016"); // while, then several statements

  {
    std::cout << "SAMPLE:\n";
    TokenStack stack;
    tokenize_string("while(1){3;}while(1)3;", stack);
    NodeVisitorOut visitor(std::cout, false);
    parse(stack, visitor);
  }

  return 0;
}
