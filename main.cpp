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

  constexpr int n_keywords = 24;
  constexpr int n_tokens = n_keywords + 2;
  constexpr int ind_token_num = n_keywords + 0;
  constexpr int ind_token_id =  n_keywords + 1;
  const string token_id_stops = "+=!(){}?:;&|-*/";
  const auto is_id = [&token_id_stops](const char c) { return token_id_stops.find(c) == string::npos; };
  const array<string, n_keywords> token_strs{
      "+", "+=", "!",    "!=",     "=",  "==",    "(",     ")",
      "{", "}",  "func", "return", "if", "while", "print", "else",
      "?", ":",  ";",    "&&",     "-",  "*",     "/",     "||",
  };
  array<int, n_tokens> token_matches;
  array<int, n_tokens> token_matches_next;
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
        if (len_match_num >= 0 && isdigit(c) && !(len_match_num == 0 && c == '0')) {
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
      if (is_ambigious) {
        if (verbose_level >= 1)
          cout << " ambigious call!\n";
      } else if (token_ind_best == -1) {
        if (verbose_level >= 1)
          cout << " undefined token!\n";
      } else if (token_ind_best < ind_token_num) {
        os_tokens << token_strs[token_ind_best] << "\n";
        if (verbose_level >= 1)
          cout << " '" << token_strs[token_ind_best] << "'\n";
      } else {
        // if rollbacked, then inc pos back to substr
        const bool ruined_all = (-1 == *max_element(token_matches_next.begin(), token_matches_next.end()));
        const int str_end = i + 1 + ruined_all;
        const int str_len = token_match_best;
        const string substr = s.substr(str_end - str_len, str_len);
        os_tokens << substr << "\n";
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
  if (verbose) {
      std::cout << "DEBUG:\n"; 
      tokenize_test_in(in, 10);
  }
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
  // // run_test("009"); // err handling

  run_test("010"); // nums complex
  run_test("011"); // nums w/ spaces
  run_test("012"); // nums w/o spaces
  run_test("013"); // ids
  run_test("014"); // ids vs keywords
  run_test("015"); // ids and nums w/o space

  return 0;
}