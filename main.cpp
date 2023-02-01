#include <algorithm>
#include <array>
#include <fstream>
#include <iostream>
#include <sstream>
#include <string>
#include <vector>

enum {
  ERR_NO,
  ERR_CANT_READ_FILE,
  ERR_FILE_IS_EMPTY,
};

int tokenize2(std::istream &ss, std::ostream &os_tokens,
              int verbose_level = 0) {
  using std::array;
  using std::cout;
  using std::fill;
  using std::getline;
  using std::iswspace;
  using std::max_element;
  using std::string;

  constexpr int nTokens = 24;
  const array<string, nTokens> token_strs{
      "+", "+=", "!",    "!=",     "=",  "==",    "(",     ")",
      "{", "}",  "func", "return", "if", "while", "print", "else",
      "?", ":",  ";",    "&&",     "-",  "*",     "/",     "||",
  };
  array<int, nTokens> token_matches;
  array<int, nTokens> token_matches_next;
  fill(token_matches.begin(), token_matches.end(), 0);

  string s;
  while (getline(ss, s)) {
    const int len = s.size();
    if (verbose_level >= 2)
      cout << "next is line '" << s << "' (" << len << ")\n";
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
      for (int token_ind = 0; token_ind < nTokens; ++token_ind) {
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
      for (int token_ind = 0; token_ind < nTokens; ++token_ind) {
        if (token_matches[token_ind] == token_match_best &&
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
      }
      if (verbose_level >= 1)
        cout << "it's a...";
      if (is_ambigious) {
        if (verbose_level >= 1)
          cout << " ambigious call!\n";
      } else if (token_ind_best == -1) {
        if (verbose_level >= 1)
          cout << " undefined token!\n";
      } else {
        os_tokens << token_strs[token_ind_best] << "\n";
        if (verbose_level >= 1)
          cout << " '" << token_strs[token_ind_best] << "'\n";
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

std::pair<int, std::string> tokenize_string(std::string const &str) {
  std::stringstream in(str);
  std::stringstream out;
  const int code = tokenize2(in, out);
  return {code, out.str()};
}

std::pair<int, std::string> tokenize_test_in(std::string const &filename,
                                             int verbose_level = 0) {
  std::ifstream in;
  in.open(filename, std::ios::binary);
  if (!in)
    return {ERR_CANT_READ_FILE, ""};
  std::stringstream out;
  const int code = tokenize2(in, out, verbose_level);
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

int run_test(const std::string &num) {
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
  if (verbose)
    std::cout << "DEBUG:\n";
  tokenize_test_in(in, 10);
  return 1;
}

int main() {
  run_test("001");
  run_test("002");
  run_test("003");
  run_test("004");
  run_test("005");
  run_test("006");
  run_test("007");
  run_test("008");
  // run_test("009");
  return 0;
}