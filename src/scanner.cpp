//------------------------------------------------------------------------------
/// @brief SnuPL/0 scanner
/// @author Bernhard Egger <bernhard@csap.snu.ac.kr>
/// @section changelog Change Log
/// 2012/09/14 Bernhard Egger created
/// 2013/03/07 Bernhard Egger adapted to SnuPL/0
/// 2014/09/10 Bernhard Egger assignment 1: scans SnuPL/-1
/// 2016/03/13 Bernhard Egger assignment 1: adapted to modified SnuPL/-1 syntax
///
/// @section license_section License
/// Copyright (c) 2012-2016, Bernhard Egger
/// All rights reserved.
///
/// Redistribution and use in source and binary forms,  with or without modifi-
/// cation, are permitted provided that the following conditions are met:
///
/// - Redistributions of source code must retain the above copyright notice,
///   this list of conditions and the following disclaimer.
/// - Redistributions in binary form must reproduce the above copyright notice,
///   this list of conditions and the following disclaimer in the documentation
///   and/or other materials provided with the distribution.
///
/// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
/// AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING,  BUT NOT LIMITED TO,  THE
/// IMPLIED WARRANTIES OF MERCHANTABILITY  AND FITNESS FOR A PARTICULAR PURPOSE
/// ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT HOLDER  OR CONTRIBUTORS BE
/// LIABLE FOR ANY DIRECT,  INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSE-
/// QUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF  SUBSTITUTE
/// GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
/// HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN  CONTRACT, STRICT
/// LIABILITY, OR TORT  (INCLUDING NEGLIGENCE OR OTHERWISE)  ARISING IN ANY WAY
/// OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
/// DAMAGE.
//------------------------------------------------------------------------------

#include <iostream>
#include <sstream>
#include <cctype>
#include <cstdlib>
#include <cstring>
#include <cassert>
#include <cstdio>

#include "scanner.h"
using namespace std;

//------------------------------------------------------------------------------
// token names
//
#define TOKEN_STRLEN 18

char ETokenName[][TOKEN_STRLEN] = {
  "tIdentifier",                    ///< an identifier
  "tNumber",                        ///< a number
  "tBoolean",                       ///< a boolean ("true", "false")
  "tCharacter",                     ///< a character
  "tString",                        ///< a string
  "tBaseType",                      ///< a base type ("integer", "char", "boolean")

  "tPlusMinus",                     ///< '+' or '-'
  "tMulDiv",                        ///< '*' or '/'
  "tAnd",                           ///< '&&'
  "tOr",                            ///< '||'
  "tNot",                           ///< '!'
  "tRelOp",                         ///< relational operator
  "tAssign",                        ///< assignment operator
  "tSemicolon",                     ///< a semicolon
  "tColon",                         ///< a colon
  "tDot",                           ///< a dot
  "tComma",                         ///< a comma
  "tLBrak",                         ///< a left bracket ('[')
  "tRBrak",                         ///< a right bracket (']')
  "tLParen",                        ///< a left parentheses ('(')
  "tRParen",                        ///< a right parentheses (')')

  "tKModule",                       ///< keyword "module"
  "tKBegin",                        ///< keyword "begin"
  "tKEnd",                          ///< keyword "end"
  "tKIf",                           ///< keyword "if"
  "tKThen",                         ///< keyword "then"
  "tKElse",                         ///< keyword "else"
  "tKWhile",                        ///< keyword "while"
  "tKDo",                           ///< keyword "do"
  "tKReturn",                       ///< keyword "return"
  "tKVar",                          ///< keyword "var"
  "tKProcedure",                    ///< keyword "procedure"
  "tKFunction",                     ///< keyword "function"

  "tEOF",                           ///< end of file
  "tIOError",                       ///< I/O error
  "tUndefined",                     ///< undefined
};


//------------------------------------------------------------------------------
// format strings used for printing tokens
//

char ETokenStr[][TOKEN_STRLEN] = {
  "tIdentifier (%s)",               ///< an identifier
  "tNumber (%s)",                   ///< a number
  "tBoolean (%s)",                  ///< a boolean ("true", "false")
  "tCharacter (%s)",                ///< a character
  "tString (%s)",                   ///< a string
  "tBaseType (%s)",                 ///< a base type ("integer", "char", "boolean")
  
  "tPlusMinus (%s)",                ///< '+' or '-'
  "tMulDiv (%s)",                   ///< '*' or '/'
  "tAnd",                           ///< '&&'
  "tOr",                            ///< '||'
  "tNot",                           ///< '!'
  "tRelOp (%s)",                    ///< relational operator
  "tAssign",                        ///< assignment operator
  "tSemicolon",                     ///< a semicolon
  "tColon",                         ///< a colon
  "tDot",                           ///< a dot
  "tComma",                         ///< a comma
  "tLBrak",                         ///< a left bracket ('[')
  "tRBrak",                         ///< a right bracket (']')
  "tLParen",                        ///< a left parentheses ('(')
  "tRParen",                        ///< a right parentheses (')')

  "tKModule",                       ///< keyword "module"
  "tKBegin",                        ///< keyword "begin"
  "tKEnd",                          ///< keyword "end"
  "tKIf",                           ///< keyword "if"
  "tKThen",                         ///< keyword "then"
  "tKElse",                         ///< keyword "else"
  "tKWhile",                        ///< keyword "while"
  "tKDo",                           ///< keyword "do"
  "tKReturn",                       ///< keyword "return"
  "tKVar",                          ///< keyword "var"
  "tKProcedure",                    ///< keyword "procedure"
  "tKFunction",                     ///< keyword "function"

  "tEOF",                           ///< end of file
  "tIOError",                       ///< I/O error
  "tUndefined (%s)"                 ///< undefined
};


//------------------------------------------------------------------------------
// reserved keywords
//
pair<const char*, EToken> Keywords[] =
{
    make_pair("module",     tKModule),
    make_pair("begin",      tKBegin),
    make_pair("end",        tKEnd),
    make_pair("if",         tKIf),
    make_pair("then",       tKThen),
    make_pair("else",       tKElse),
    make_pair("while",      tKWhile),
    make_pair("do",         tKDo),
    make_pair("return",     tKReturn),
    make_pair("var",        tKVar),
    make_pair("procedure",  tKProcedure),
    make_pair("function",   tKFunction),
    make_pair("true",       tBoolean),
    make_pair("false",      tBoolean),
    make_pair("integer",    tBaseType),
    make_pair("char",       tBaseType),
    make_pair("boolean",    tBaseType)
};



//------------------------------------------------------------------------------
// CToken
//
CToken::CToken()
{
  _type = tUndefined;
  _value = "";
  _line = _char = 0;
}

CToken::CToken(int line, int charpos, EToken type, const string value)
{
  _type = type;
  _value = value; 
  /** @note value should be escaped before passed to this constructor.
   *        i.e. value: "He\'s a genius." (O) / "He's a genius" (X)
   */        
  _line = line;
  _char = charpos;
}

CToken::CToken(const CToken &token)
{
  _type = token.GetType();
  _value = token.GetValue();
  _line = token.GetLineNumber();
  _char = token.GetCharPosition();
}

CToken::CToken(const CToken *token)
{
  _type = token->GetType();
  _value = token->GetValue();
  _line = token->GetLineNumber();
  _char = token->GetCharPosition();
}

const string CToken::Name(EToken type)
{
  return string(ETokenName[type]);
}

const string CToken::GetName(void) const
{
  return string(ETokenName[GetType()]);
}

ostream& CToken::print(ostream &out) const
{
  int str_len = _value.length();
  str_len = TOKEN_STRLEN + (str_len < 64 ? str_len : 64);
  char *str = (char*)malloc(str_len);
  snprintf(str, str_len, ETokenStr[GetType()], _value.c_str());
  out << dec << _line << ":" << _char << ": " << str;
  free(str);
  return out;
}

string CToken::escape(const string text)
{
  const char *t = text.c_str();
  string s;

  while (*t != '\0') {
    switch (*t) {
      case '\n': s += "\\n";  break;
      case '\t': s += "\\t";  break;
      case '\0': s += "\\0";  break;
      case '\'': s += "\\'";  break;
      case '\"': s += "\\\""; break;
      case '\\': s += "\\\\"; break;
      default :  s += *t;
    }
    t++;
  }

  return s;
}

string CToken::unescape(const string text)
{
  const char *t = text.c_str();
  string s;

  while (*t != '\0') {
    if (*t == '\\') {
      switch (*++t) {
        case 'n': s += "\n";  break;
        case 't': s += "\t";  break;
        case '0': s += "\0";  break;
        case '\'': s += "'";  break;
        case '"': s += "\""; break;
        case '\\': s += "\\"; break;
        default :  s += '?';
      }
    } else {
      s += *t;
    }
    t++;
  }

  return s;
}

ostream& operator<<(ostream &out, const CToken &t)
{
  return t.print(out);
}

ostream& operator<<(ostream &out, const CToken *t)
{
  return t->print(out);
}


//------------------------------------------------------------------------------
// CScanner
//
map<string, EToken> CScanner::keywords;

CScanner::CScanner(istream *in)
{
  InitKeywords();
  _in = in;
  _delete_in = false;
  _line = _char = 1;
  _token = NULL;
  _good = in->good();
  NextToken();
}

CScanner::CScanner(string in)
{
  InitKeywords();
  _in = new istringstream(in);
  _delete_in = true;
  _line = _char = 1;
  _token = NULL;
  _good = true;
  NextToken();
}

CScanner::~CScanner()
{
  if (_token != NULL) delete _token;
  if (_delete_in) delete _in;
}

void CScanner::InitKeywords(void)
{
  if (keywords.size() == 0) {
    int size = sizeof(Keywords) / sizeof(Keywords[0]);
    for (int i=0; i<size; i++) {
      keywords[Keywords[i].first] = Keywords[i].second;
    }
  }
}

CToken CScanner::Get()
{
  CToken result(_token);

  EToken type = _token->GetType();
  _good = !(type == tIOError);

  NextToken();
  return result;
}

CToken CScanner::Peek() const
{
  return CToken(_token);
}

void CScanner::NextToken()
{
  if (_token != NULL) delete _token;

  _token = Scan();
}

void CScanner::RecordStreamPosition()
{
  _saved_line = _line;
  _saved_char = _char;
}

void CScanner::GetRecordedStreamPosition(int *lineno, int *charpos)
{
  *lineno = _saved_line;
  *charpos = _saved_char;
}

CToken* CScanner::NewToken(EToken type, const string token)
{
  return new CToken(_saved_line, _saved_char, type, token);
}

CToken* CScanner::Scan()
{
  EToken token;
  string tokval;
  char c;

  while (_in->good() && IsWhite(_in->peek())) GetChar();

  RecordStreamPosition();

  if (_in->eof()) return NewToken(tEOF);
  if (!_in->good()) return NewToken(tIOError);

  c = GetChar();
  tokval = c;
  token = tUndefined;

  switch (c) {
    case ':':
      if (_in->peek() == '=') {
        tokval += GetChar();
        token = tAssign;
      } else {
        token = tColon;
      }
      break;

    case '+':
    case '-':
      token = tPlusMinus;
      break;

    /** @note \b Handling \b Comments
     *  @code
             case '/':
               if (_in->peek() == '/') { 
                 while (!_in->eof() && (c = GetChar()) != '\n'); // Consumes comments until newline or EOF has appeared.
                 return Scan();                                  // Recursive call of Scan(): make new token from next line.
               }
        @endcode
     */

    case '/':
      if (_in->peek() == '/') { 
        while (!_in->eof() && (c = GetChar()) != '\n');
        return Scan();
      }
    case '*':
      token = tMulDiv;
      break;

    case '=':
    case '#':
      token = tRelOp;
      break;

    case '<':
    case '>':
      if (_in->peek() == '=') {
        tokval += GetChar();
      }
      token = tRelOp;
      break;

    case '!':
      token = tNot;
      break;

    case '&':
      if (_in->peek() == '&') {
        tokval += GetChar();
        token = tAnd;
      } else {
        token = tUndefined;
      }
      break;

    case '|':
      if (_in->peek() == '|') {
        tokval += GetChar();
        token = tOr;
      } else {
        token = tUndefined;
      }
      break;

    case ';':
      token = tSemicolon;
      break;

    case '.':
      token = tDot;
      break;

    case ',':
      token = tComma;
      break;

    case '(':
      token = tLParen;
      break;

    case ')':
      token = tRParen;
      break;

    case '[':
      token = tLBrak;
      break;

    case ']':
      token = tRBrak;
      break;

      
    /** @note \b Handling \b Character \b Literals
     *  @code
            case '\'':
              tokval = "";
              if (_in->peek() == '\\') {
                  tokval += GetChar();
                  char lookahead = _in->peek();
                  if (   lookahead == 'n'
                      || lookahead == 't'
                      || lookahead == '\"'
                      || lookahead == '\''
                      || lookahead == '\\'
                      || lookahead == '0'  ) {

                      tokval += GetChar();
                  } 
                  else {
                      tokval = "\'" + tokval;
                      break;                    // break from switch statement
                  } 
              } 
              else if (_in->peek() == '\'') { // when the input is '' (no character in single quotes)
                  GetChar();                    // Consumes the closing single quote
                  tokval = "\'" + tokval;
                  break;
              } 
              else {
                  if (false == IsASCIIChar(_in->peek())) break;
                  else tokval += GetChar();
              }
              
              if (_in->peek() == '\'') {
                  token = tCharacter;
                  GetChar();                    // Consumes the closing single quote.
              } 
              else 
                  tokval = "\'" + tokval;

              break;
        @endcode
     */
    case '\'':
      tokval = "";
      if (_in->peek() == '\\') {
          tokval += GetChar();
          char lookahead = _in->peek();
          if (   lookahead == 'n'
              || lookahead == 't'
              || lookahead == '\"'
              || lookahead == '\''
              || lookahead == '\\'
              || lookahead == '0'  ) {

              tokval += GetChar();
          } else {
              tokval = "\'" + tokval;
              break; // break from switch statement
          } 
      } else if (_in->peek() == '\'') { // when the input is '' (no character in single quotes)
          GetChar(); // Consumes the closing single quote
          tokval = "\'" + tokval;
          break;
      } else {
          if (false == IsASCIIChar(_in->peek())) break;
          else tokval += GetChar();
      }
      
      if (_in->peek() == '\'') {
          token = tCharacter;
          GetChar(); // Consumes the closing single quote.
      } else 
          tokval = "\'" + tokval;

      break;

    /** @note \b Handling \b Comments
     *  @code
            case '\"':
              tokval = "";
              while (_in->good()) {
                  if (_in->peek() != '\"' || c == '\\') {
                      if (false == IsASCIIChar(_in->peek())) break;
                      c = GetChar();
                      tokval += c;
                  } 
                  else {
                      token = tString;
                      GetChar();        // Consumes closing quote.
                      break;
                  }
              }
              if(token != tString) 
                  tokval = "\"" + tokval;
              break;
        @endcode
     */
    case '\"':
      tokval = "";
      while (_in->good()) {
          if (_in->peek() != '\"' || c == '\\') {
              if (false == IsASCIIChar(_in->peek())) break;
              c = GetChar();
              tokval += c;
          } else {
              token = tString;
              GetChar();        // Consumes closing quote.
              break;
          }
      }
      if(token != tString) 
          tokval = "\"" + tokval;
      break;

    default:
      if (('0' <= c) && (c <= '9')) {
        token = tNumber;
        while ('0' <= _in->peek() && _in->peek() <= '9') 
            tokval += GetChar();
      } else if (   (('a' <= c) && (c <= 'z'))
                ||  (('A' <= c) && (c <= 'Z'))
                ||   c == '_'                    ){
        token = tIdentifier;
        char lookahead = _in->peek();
        while (   (('0' <= lookahead) && (lookahead <= '9'))
              ||  (('A' <= lookahead) && (lookahead <= 'Z'))
              ||  (('a' <= lookahead) && (lookahead <= 'z'))
              ||  lookahead == '_'                          ) {
            tokval += GetChar();
            lookahead = _in->peek();
        }

        /** @note \b Handling \b Keywords \b and \b Reserved \b Words
         *  @code
                // Check if this token is a keyword.
                map<string, EToken>::iterator resultItr = keywords.find(tokval);
                if (resultItr != keywords.end())
                    token = resultItr->second;
            @endcode
         */
        // Check if this token is a keyword.
        map<string, EToken>::iterator resultItr = keywords.find(tokval);
        if (resultItr != keywords.end())
            token = resultItr->second;

      } else {
        tokval = "invalid character '";
        tokval += c;
        tokval += "'";
      }
      break;
  }

  return NewToken(token, tokval);
}

char CScanner::GetChar()
{
  char c = _in->get();
  if (c == '\n') { _line++; _char = 1; } else _char++;
  return c;
}

string CScanner::GetChar(int n)
{
  string str;
  for (int i=0; i<n; i++) str += GetChar();
  return str;
}

bool CScanner::IsWhite(char c) const
{
  return ((c == ' ') || (c == '\n'));
}

bool CScanner::IsASCIIChar(char c) const
{
    return ((c >= 0x20) && (c < 0x7f));
}
