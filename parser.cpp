//------------------------------------------------------------------------------
/// @brief SnuPL/0 parser
/// @author Bernhard Egger <bernhard@csap.snu.ac.kr>
/// @section changelog Change Log
/// 2012/09/14 Bernhard Egger created
/// 2013/03/07 Bernhard Egger adapted to SnuPL/0
/// 2014/11/04 Bernhard Egger maintain unary '+' signs in the AST
/// 2016/04/01 Bernhard Egger adapted to SnuPL/1 (this is not a joke)
/// 2016/09/28 Bernhard Egger assignment 2: parser for SnuPL/-1
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

#include <limits.h>
#include <cassert>
#include <errno.h>
#include <cstdlib>
#include <vector>
#include <iostream>
#include <exception>

#include "parser.h"
using namespace std;


//------------------------------------------------------------------------------
// CParser
//
CParser::CParser(CScanner *scanner)
{
  _scanner = scanner;
  _module = NULL;
}

CAstNode* CParser::Parse(void)
{
  _abort = false;

  if (_module != NULL) { delete _module; _module = NULL; }

  try {
    if (_scanner != NULL) _module = module();

    if (_module != NULL) {
      CToken t;
      string msg;
      if (!_module->TypeCheck(&t, &msg)) SetError(t, msg);
    }
  } catch (...) {
    _module = NULL;
  }

  return _module;
}

const CToken* CParser::GetErrorToken(void) const
{
  if (_abort) return &_error_token;
  else return NULL;
}

string CParser::GetErrorMessage(void) const
{
  if (_abort) return _message;
  else return "";
}

void CParser::SetError(CToken t, const string message)
{
  _error_token = t;
  _message = message;
  _abort = true;
  throw message;
}

bool CParser::Consume(EToken type, CToken *token)
{
  if (_abort) return false;

  CToken t = _scanner->Get();

  if (t.GetType() != type) {
    SetError(t, "expected '" + CToken::Name(type) + "', got '" +
             t.GetName() + "'");
  }

  if (token != NULL) *token = t;

  return t.GetType() == type;
}

void CParser::InitSymbolTable(CSymtab *s)
{
  CTypeManager *tm = CTypeManager::Get();

  // DIM(a: pointer to array, dim: integer)
  CSymProc *DIM_symbol = new CSymProc("DIM", tm->GetInt());
  DIM_symbol->AddParam(new CSymParam(0, "a", tm->GetPointer(tm->GetNull()))); 
  DIM_symbol->AddParam(new CSymParam(1, "dim", tm->GetInt()));
  s->AddSymbol(DIM_symbol);

  // DOFS(a: pointer to array)
  CSymProc *DOFS_symbol = new CSymProc("DOFS", tm->GetInt());
  DOFS_symbol->AddParam(new CSymParam(0, "a", tm->GetPointer(tm->GetNull()))); 
  // Note: using "Pointer to type 'null'" to indicate a pointer to an arbitrary array.
  s->AddSymbol(DOFS_symbol);

  // ReadInt()
  CSymProc *ReadInt_symbol = new CSymProc("ReadInt", tm->GetInt());
  s->AddSymbol(ReadInt_symbol);

  // WriteInt(i: integer)
  CSymProc *WriteInt_symbol = new CSymProc("WriteInt", tm->GetNull()); 
  WriteInt_symbol->AddParam(new CSymParam(0, "i", tm->GetInt()));
  s->AddSymbol(WriteInt_symbol);

  // WriteChar(c: char)
  CSymProc *WriteChar_symbol = new CSymProc("WriteChar", tm->GetNull());
  WriteChar_symbol->AddParam(new CSymParam(0, "c", tm->GetChar()));
  s->AddSymbol(WriteChar_symbol);

  // WriteStr(str: char[])
  CSymProc *WriteStr_symbol = new CSymProc("WriteStr", tm->GetNull());
  WriteStr_symbol->AddParam(new CSymParam(0, "str", tm->GetPointer(tm->GetArray(CArrayType::OPEN, tm->GetChar()))));
  s->AddSymbol(WriteStr_symbol);

  // WriteLn()
  CSymProc *WriteLn_symbol = new CSymProc("WriteLn", tm->GetNull());
  s->AddSymbol(WriteLn_symbol); 
}

CAstModule* CParser::module(void)
{
  // SnuPL/-1:
  //    module ::= statSequence  ".".
  // 
  // SnuPL/1:
  //    module ::= "module" ident ";" varDeclaration { subroutineDecl } "begin" statSequence "end" ident ".".
  CToken t, moduleIdent;
  EToken tt;
  CAstModule *m;
  CAstStatement *statseq = NULL;

  Consume(tKModule);
  Consume(tIdentifier, &moduleIdent);
  m = new CAstModule(moduleIdent, moduleIdent.GetValue());
  Consume(tSemicolon);

  InitSymbolTable(m->GetSymbolTable());

  varDeclaration(m);
  while(!_abort)
  {
    tt = _scanner->Peek().GetType();
    if(tt == tKBegin) break;
    else if (tt != tKProcedure && tt != tKFunction)
    {
      SetError(_scanner->Peek(), "next token not in FIRST(subroutineDecl)");
      break;
    }

    subroutineDecl(m);
  }

  Consume(tKBegin);
  statseq = statSequence(m);
  Consume(tKEnd);
  m->SetStatementSequence(statseq);

  Consume(tIdentifier, &t);
  if(t.GetValue().compare(moduleIdent.GetValue()) != 0)
      SetError(t, "module name mismatch: expected " + moduleIdent.GetValue() + ", got " + t.GetValue());
  Consume(tDot);

  return m;
}

CAstStatement* CParser::statSequence(CAstScope *s)
{
  // SnuPL/-1:
  //  statSequence ::= [ statement { ";" statement } ].
  //  statement ::= assignment.
  //  FIRST(statSequence) = { tNumber }
  //  FOLLOW(statSequence) = { tDot }
  //
  // SnuPL/1:
  //  statSequence ::= [ statement { ";" statement } ].
  //  statement ::= assignment | subroutineCall | ifStatement | whileStatement | returnStatement.
  //  
  //  FIRST(statSequence) = { tIdentifier, tKIf, tKWhile, tKReturn, epsilon }
  //  FOLLOW(statSequence) = { tKEnd, tKElse }
  //
  //  FIRST(statement) = { tIdentifier, tKIf, tKWhile, tKReturn }
  //  FOLLOW(statement) = { tSemicolon, tKEnd, tKElse }
  //

  CAstStatement *head = NULL;

  EToken tt = _scanner->Peek().GetType();
  if (!(tt == tKEnd || tt == tKElse)) {
    CAstStatement *tail = NULL;

    do {
      CToken t;
      EToken tt = _scanner->Peek().GetType();
      CAstStatement *st = NULL;

      switch (tt) {
        // statement ::= assignment | subroutineCall
        case tIdentifier:
          Consume(tIdentifier, &t);
          tt = _scanner->Peek().GetType();
          if(tt == tLParen)
            st = new CAstStatCall(t, subroutineCall(s, t));
          else
            st = assignment(s, t);
          break;
        // statement ::= ifStatement
        case tKIf:
          st = ifStatement(s);
          break;
        // statement ::= whileStatement
        case tKWhile:
          st = whileStatement(s);
          break;
        // statement ::= returnStatement
        case tKReturn:
          st = returnStatement(s);
          break;

        default:
          SetError(_scanner->Peek(), "statement expected.");
          break;
      }

      assert(st != NULL);
      if (head == NULL) head = st;
      else tail->SetNext(st);
      tail = st;

      tt = _scanner->Peek().GetType();
      if (tt == tKElse || tt == tKEnd) break;

      Consume(tSemicolon);
    } while (!_abort);
  }

  return head;
}

CAstStatAssign* CParser::assignment(CAstScope *s, CToken ident)
{
  // SnuPL/-1:
  //    assignment ::= number ":=" expression.
  //
  // SnuPL/1:
  //    assignment ::= qualident ":=" expression.
  CToken t;

  CAstDesignator *lhs = qualident(s, ident);
  Consume(tAssign, &t);

  CAstExpression *rhs = expression(s);

  return new CAstStatAssign(t, lhs, rhs);
}

CAstExpression* CParser::expression(CAstScope* s)
{
  //
  // expression ::= simpleexpr [ relOp simpleexpr ].
  //
  // FIRST(expression) = { tPlusMinus, tIdentifier, tNumber, tBoolean, tCharacter, tString, tLParen, tNot}
  // FOLLOW(expression) = { tRBrak, tRParen, tComma, tSemicolon, tKElse, tKEnd }
  // 

  CToken t;
  EOperation relop;
  CAstExpression *left = NULL, *right = NULL;

  left = simpleexpr(s);

  if (_scanner->Peek().GetType() == tRelOp) {
    Consume(tRelOp, &t);
    right = simpleexpr(s);

    if (t.GetValue() == "=")       relop = opEqual;
    else if (t.GetValue() == "#")  relop = opNotEqual;
    else if (t.GetValue() == "<")  relop = opLessThan;
    else if (t.GetValue() == "<=") relop = opLessEqual;
    else if (t.GetValue() == ">")  relop = opBiggerThan;
    else if (t.GetValue() == ">=") relop = opBiggerEqual;
    else SetError(t, "invalid relation.");

    return new CAstBinaryOp(t, relop, left, right);
  } else {
    return left;
  }
}

CAstExpression* CParser::simpleexpr(CAstScope *s)
{
  // SnuPL/-1
  //  simpleexpr ::= term { termOp term }.
  //
  // SnuPL/1:
  //  simpleexpr ::= ["+"|"-"] term { termOp term }.
  //
  // FIRST(simpleexpr) = { tPlusMinus, tIdentifier, tNumber, tBoolean, tCharacter, tString, tLParen, tNot}
  // FOLLOW(simpleexpr) = { tRelOp, tRBrak, tRParen, tComma, tSemicolon, tKElse, tKEnd }
  CAstExpression *n = NULL;
  bool unaryFlag = false;
  EOperation unaryOp;
  CToken unaryToken;
  EToken tt;

  // handling unary operators
  if(_scanner->Peek().GetType() == tPlusMinus)
  {
    Consume(tPlusMinus, &unaryToken);
    if(unaryToken.GetValue() == "+") unaryOp = opPos;
    else if(unaryToken.GetValue() == "-") unaryOp = opNeg;
    unaryFlag = true;
  }

  n = term(s);
  if(unaryFlag) n = new CAstUnaryOp(unaryToken, unaryOp, n);

  tt = _scanner->Peek().GetType();
  while (tt == tPlusMinus || tt == tOr) {
    CToken t;
    CAstExpression *l = n, *r;

    if(tt == tPlusMinus) Consume(tPlusMinus, &t);
    else Consume (tOr, &t);

    r = term(s);

    if(tt == tPlusMinus) 
      n = new CAstBinaryOp(t, t.GetValue() == "+" ? opAdd : opSub, l, r);
    else
      n = new CAstBinaryOp(t, opOr, l, r);
    tt = _scanner->Peek().GetType();
  }

  return n;
}

CAstExpression* CParser::term(CAstScope *s)
{
  //
  // term ::= factor { factOp factor }.
  //
  // FIRST(term) = { tIdentifier, tNumber, tBoolean, tCharacter, tString, tLParen, tNot}
  // FOLLOW(term) = { tPlusMinus, tOr, tRBrak, tRParen, tComma, tSemicolon, tKElse, tKEnd }
  //
  CAstExpression *n = NULL;

  n = factor(s);

  EToken tt = _scanner->Peek().GetType();

  while ((tt == tMulDiv) || (tt == tAnd)) {
    CToken t;
    CAstExpression *l = n, *r;

    if(tt == tMulDiv) Consume(tMulDiv, &t);
    else Consume(tAnd, &t);

    r = factor(s);

    if(tt == tMulDiv)
      n = new CAstBinaryOp(t, t.GetValue() == "*" ? opMul : opDiv, l, r);
    else
      n = new CAstBinaryOp(t, opAnd, l, r);

    tt = _scanner->Peek().GetType();
  }

  return n;
}

CAstExpression* CParser::factor(CAstScope *s)
{
  // SnuPL/-1:
  //  factor ::= number | "(" expression ")"
  //
  //  FIRST(factor) = { tNumber, tLBrak }
  //
  // SnuPL/1:
  //  factor ::= qualident | number | boolean | char | string | "(" expression ")" 
  //            | subroutineCall | "!" factor.
  //
  //  FIRST(factor) = { tIdent, tNumber, tBoolean, tChar, tString, tLParen, tNot }
  //  FOLLOW(factor) = { tMulDiv, tAnd, tPlusMinus, tOr, tRelOp, tRBrak, tRParen, tComma, tSemicolon, tKElse, tKEnd }
  //

  CToken t;
  EToken tt = _scanner->Peek().GetType();
  CAstExpression *unary = NULL, *n = NULL;

  switch (tt) {
    // factor ::= number
    case tNumber:
      n = number();
      break;
    
    // factor ::= booelan
    case tBoolean:
      n = boolean();
      break;

    // factor ::= char
    case tCharacter:
      n = charConstant();
      break;

    // factor ::= string
    case tString:
      n = stringConstant(s);
      break;

    // factor ::= "!" factor
    case tNot:
      Consume(tNot, &t);
      unary = factor(s);
      n = new CAstUnaryOp(t, opNot, unary);
      break;

    // factor ::= "(" expression ")"
    case tLParen:
      Consume(tLParen);
      n = expression(s);
      Consume(tRParen);
      break;

    // factor ::= qualident | subroutineCall
    case tIdentifier:
      Consume(tIdentifier, &t);
      if(_scanner->Peek().GetType() == tLParen)
        n = subroutineCall(s, t);
      else
        n = qualident(s, t);
      break;     

    default:
      cout << "got " << _scanner->Peek() << endl;
      SetError(_scanner->Peek(), "factor expected.");
      break;
  }

  return n;
}

CAstConstant* CParser::number(void)
{
  //
  // number ::= digit { digit }.
  //
  // "digit { digit }" is scanned as one token (tNumber)
  // 
  // Semantic Check: range check for 0 and (INT_MAX+1)

  CToken t;

  Consume(tNumber, &t);

  errno = 0;
  long long v = strtoll(t.GetValue().c_str(), NULL, 10);
  if (errno != 0) SetError(t, "invalid number.");

  if(v < 0LL || v > (1LL+INT_MAX))
    SetError(t, "number out of range");

  return new CAstConstant(t, CTypeManager::Get()->GetInt(), v);
}

CAstConstant* CParser::boolean(void)
{
  //
  // boolean ::= "true" | "false".
  
  CToken t;
  
  Consume(tBoolean, &t);
  
  return new CAstConstant(t, CTypeManager::Get()->GetBool(), (t.GetValue() == "true" ? 1LL : 0LL));
}

CAstConstant* CParser::charConstant(void)
{
  //
  // char ::= "'" character "'".
  // character ::= printable ASCIIchar | "\n" | "\t" | "\"" | "\'" | "\\" | "\0".

  CToken t;
  Consume(tCharacter, &t);

  return new CAstConstant(t, CTypeManager::Get()->GetChar(), (CToken::unescape(t.GetValue())[0]));
}

CAstStringConstant* CParser::stringConstant(CAstScope *s)
{
  //
  // string ::= '"' { character } '"'.
  // character ::= printable ASCIIchar | "\n" | "\t" | "\"" | "\'" | "\\" | "\0".
  

  CToken t;
  Consume(tString, &t);

  return new CAstStringConstant(t, t.GetValue(), s);
}


CAstDesignator* CParser::qualident(CAstScope *s, CToken ident)
{
    //
    // qualident ::= ident { "[" expression "]" }.
    // FIRST(qualident) = { tIdentifier }
    // FOLLOW(qualident) = { tAssign, tMulDiv, tAnd, 
    //                      tPlusMinus, tOr, tRelOp,
    //                      tRParen, tRBrak, tComma,
    //                      tSemicolon, tKElse, tKEnd }
    CToken t;
    EToken tt = _scanner->Peek().GetType();
    CAstDesignator *n = NULL;
    CAstArrayDesignator *n_array = NULL;

    /*
    // acquiring ident
    if(!assign) 
    {
      if(tt == tIdentifier) Consume(tIdentifier, &t);
      else { SetError(_scanner->Peek(), "ident expected"); return NULL; }
    }
    else
    */
    t = ident;
    const CSymbol *desig_sym = (s->GetSymbolTable())->FindSymbol(t.GetValue());

    // Check 'declaration before use'
    if(desig_sym == NULL)
    {
      SetError(t, "Variables must be declared before use.");
    }

    // Check whether this is an array designator.
    tt = _scanner->Peek().GetType();
    if(tt == tLBrak)
    {
        n_array = new CAstArrayDesignator(t, (s->GetSymbolTable())->FindSymbol(t.GetValue()));
        n = n_array;

        do
        {
            Consume(tLBrak);
            n_array->AddIndex(expression(s));
            Consume(tRBrak);

            tt = _scanner->Peek().GetType();
            if(!(tt == tLBrak)) break;
        } while(!_abort);

        n_array->IndicesComplete();
    }
    else
    {
        n = new CAstDesignator(t, (s->GetSymbolTable())->FindSymbol(t.GetValue()));
    }

    // Check whether next token is in the FOLLOW(qualident).
    switch(tt)
    {
        case tAssign:
        case tMulDiv:
        case tAnd:
        case tPlusMinus:
        case tOr:
        case tRelOp:
        case tRParen:
        case tRBrak:
        case tComma:
        case tSemicolon:
        case tKElse:
        case tKEnd:
            break;
        default:
            SetError(_scanner->Peek(), "this token is not in FOLLOW(qualident)");
            break;
    }

    return n;
}

void CParser::varDeclaration(CAstScope *s)
{
  // SnuPL/1:
  //  varDeclaration ::= [ "var" varDeclSequence ";" ].
  //  varDeclSequence ::= varDecl { ";" varDecl }.
  //  varDecl ::= ident { "," ident } ":" type.
  //
  //  FIRST(varDeclaration) = { tKVar, e }
  //  FOLLOW(varDeclaration) = { tKProcedure, tKFunction, tKBegin }

  EToken tt = _scanner->Peek().GetType();

  // If no varDeclaration exists.
  if(tt == tKProcedure || tt == tKFunction || tt == tKBegin) return;

  Consume(tKVar);
  varDeclSequence(s, NULL, false);
  //Consume(tSemicolon); <-- Already done in the varDeclSequence(s, false)

  tt = _scanner->Peek().GetType();

  // Check whether next token is in the FOLLOW(varDeclaration)
  switch(tt) 
  {
    case tKProcedure:
    case tKFunction:
    case tKBegin:
      break;
    default:
      SetError(_scanner->Peek(), "next token not in FOLLOW(varDeclaration)");
  }
}

void CParser::varDeclSequence(CAstScope *s, CSymProc *symproc, bool fp)
{
  // SnuPL/1:
  //  varDeclSequence ::= varDecl { ";" varDecl }.
  //  varDecl ::= ident { "," ident } ":" type.
  //
  //  FIRST(varDeclSequence) = { tIdentifier }
  //  FOLLOW(varDeclSequence) = { tSemicolon, tRParen }
  //
  //  FIRST(varDecl) = { tIdentifier }
  //  FOLLOW(varDecl) = { tSemicolon, tRParen }
  //  
  //  Note: The end of the varDeclSequence cannot be decided with one lookahead
  //    - If we peek 'tSemicolon', we don't know this ";" is seperater between 'varDecl's
  //      or indicating the end of the varDeclSequence
  //    - It can be resolved by two lookaheads:
  //      - If second lookahead is tIdentifier, then it is seperater between 'varDecl's
  //      - else if second lookahead is (tKProcedure | tKFunction | tKBegin), 
  //        then this is the end of the varDeclSequence.
  //      - If first lookahead is tRParen, then this is the end of varDeclSequence. (as formalParam)
  //

  CSymtab *st = s->GetSymbolTable();
  EToken tt;
  int param_index = 0;

  do // varDeclSequence-varDecl loop
  { // Start of the varDecl 
    vector<CToken> identList;

    do // varDecl-ident loop
    {
      CToken currIdent;
      Consume(tIdentifier, &currIdent);
      identList.push_back(currIdent);

      tt = _scanner->Peek().GetType();
      if(tt == tColon) break;
      else if(tt != tComma)
      {
        SetError(_scanner->Peek(), "varDecl parse error: expected ',', got " + _scanner->Peek().GetValue());
        break;
      }
      else Consume(tComma);
    } while(!_abort);

    Consume(tColon);
    
    CType *varType = type(NULL);
    if(!fp) // Check open arrays in non-formalParam varDeclSequence. 
    {
      const CType *curr_t = varType;
      while(curr_t->IsArray())
      {
        const CArrayType *curr_at = dynamic_cast<const CArrayType*>(curr_t);
        if(curr_at->GetNElem() == CArrayType::OPEN)
          SetError(identList[0], "Cannot have open arrays in variable declaration");
        
        curr_t = curr_at->GetInnerType();
      }
    }

    // varDecl-symbol creation & addition loop
    for(vector<CToken>::iterator it = identList.begin(); it != identList.end(); it++)
    {
      if(fp == true) // symbol creation for formal parameters
      {
        // Implicit type conversion of formal array parameters
        // from array<...> to ptr to array<...>
        if(varType->IsArray() == true)
          varType = (CType*)(CTypeManager::Get()->GetPointer(varType));

        CSymParam *symbol = new CSymParam(param_index, it->GetValue(), varType);
        bool result = st->AddSymbol(symbol);
        if(!result) SetError((*it), "duplicated parameter declaration in this procedure");
        symproc->AddParam(symbol);
        param_index++;
      }
      else // symbol creation for varDeclaration
      {
        CSymbol *symbol = s->CreateVar(it->GetValue(), varType);
        bool result = st->AddSymbol(symbol);
        if(!result) SetError((*it), "duplicated variable declaration in this scope");
      }
    }
    // End of the varDecl
    
    // Checking the end of varDeclSequence with two lookahead
    // (Just consuming tSemicolon)
    tt = _scanner->Peek().GetType();
    if(tt == tSemicolon)
    {
        Consume(tSemicolon);
        tt = _scanner->Peek().GetType();

        if(tt == tKProcedure || tt == tKFunction || tt == tKBegin) // End of varDeclaration
          break;
        else if (tt != tIdentifier)
          SetError(_scanner->Peek(), "varDeclSequence parse error: expected tIdentifier or FOLLOW(varDeclaration), got " + _scanner->Peek().GetValue());
    }
    else if (tt == tRParen) break; // End of varDeclSequence in formalParam
    else
      SetError(_scanner->Peek(), "varDeclSequence parse error: next token not in FOLLOW(varDeclSequence)");

  } while(!_abort);
}

CType* CParser::type(CType *basetype)
{
  // 
  // type ::= basetype | type "[" [ number ] "]".
  //
  // FIRST(type) = { tBasetype }
  // FOLLOW(type) = { tSemicolon, tRParen }
  
  CToken t;
  EToken tt;
  int array_size = CArrayType::OPEN;
  if(basetype == NULL)
  {
    Consume(tBaseType, &t);
    if(t.GetValue().compare("integer") == 0)
      basetype = (CType*)CTypeManager::Get()->GetInt();
    else if(t.GetValue().compare("boolean") == 0)
      basetype = (CType*)CTypeManager::Get()->GetBool();
    else if(t.GetValue().compare("char") == 0)
      basetype = (CType*)CTypeManager::Get()->GetChar();
    else
      SetError(t, "Wrong basetype: " + t.GetValue());
  }

  tt = _scanner->Peek().GetType();
  if(tt == tSemicolon || tt == tRParen) // End of type reached. (base case of recursion)
    return basetype;
  else if(tt != tLBrak)
    SetError(_scanner->Peek(), "type parse error: expected '[', got" + _scanner->Peek().GetValue());

  Consume(tLBrak);
  tt = _scanner->Peek().GetType();
  if(tt == tNumber) 
  {
    Consume(tNumber, &t);

    errno = 0;
    long v = strtol(t.GetValue().c_str(), NULL, 10);
    if (errno != 0 || v > INT_MAX || v <= 0)
      SetError(t, "invalid number in array dimension constants.");

    array_size = (int)v;
  }
  Consume(tRBrak);

  // To get the proper type of an element for this level of array,
  // recursive call of type() is needed.
  CType *processed_type = type(basetype);
  return (CType*)CTypeManager::Get()->GetArray(array_size, processed_type);
}

void CParser::subroutineDecl(CAstScope *s)
{
  // SnuPL/1:
  //  subroutineDecl ::= (procedureDecl | functionDecl) subroutineBody ident ";".
  //  procedureDecl ::= "procedure" ident [ formalParam ] ";".
  //  functionDecl ::= "function" ident [ formalParam ] ":" type ";".
  //  formalParam ::= "(" [ varDeclSequence ] ")".
  //  subroutineBody ::= varDeclaration "begin" statSequence "end".
  //
  //  FIRST(subroutineDecl) = { tKProcedure, tKFunction }
  //  FOLLOW(subroutineDecl) = { tKBegin, tKProcedure, tKFunction }
  //
  //  FIRST(procedureDecl) = { tKProcedure }
  //  FOLLOW(procedureDecl) = { tKVar, tKBegin }
  //
  //  FIRST(functionDecl) = { tKFunction }
  //  FOLLOW(functionDecl) = { tKVar, tKBegin }
  //
  //  FIRST(formalParam) = { tLParen }
  //  FOLLOW(formalParam) = { tSemicolon, tColon }
  //
  //  FIRST(subroutineBody) = { tKVar, tKBegin }
  //  FOLLOW(subroutineBody) = { tIdentifier }
  
  CAstProcedure *proc;
  CSymProc *symproc;
  CAstStatement *statseq = NULL;
  CToken t = _scanner->Peek();
  EToken proctype, tt = t.GetType();
  proctype = tt;

  // parsing ' ("procedure" | "function") ident '
  if(tt == tKProcedure)
  {
    Consume(tKProcedure);
    Consume(tIdentifier, &t);
    symproc = new CSymProc(t.GetValue(), CTypeManager::Get()->GetNull()); 
    proc = new CAstProcedure(t, t.GetValue(), s, symproc);
  }
  else if(tt == tKFunction)
  {
    Consume(tKFunction);
    Consume(tIdentifier, &t);
    symproc = new CSymProc(t.GetValue(), CTypeManager::Get()->GetNull()); 
    proc = new CAstProcedure(t, t.GetValue(), s, symproc);
  }
  else
    SetError(t, "next token not in FIRST(subroutineDecl)");

  // consuming formalParam
  tt = _scanner->Peek().GetType();
  if(tt == tLParen)
  {
    Consume(tLParen);
    if(_scanner->Peek().GetType() != tRParen)
      varDeclSequence(proc, symproc, true);
    Consume(tRParen);
  }

  // function return type setting
  if(proctype == tKFunction)
  {
    Consume(tColon);
    CType *ret_type = type(NULL);
    symproc->SetDataType(ret_type);
  }

  Consume(tSemicolon); // End of (procedureDecl | functionDecl)

  bool result = false;
  result = (s->GetSymbolTable())->AddSymbol(symproc); // Insert symproc to the current scope symbol table
  if(!result) SetError(t, "duplicated symbol (for this subroutineDecl) in this scope");
  /* This won't needed because if in this procedure, this cannot find itself, then it looks the parent symbol table.
  result = (proc->GetSymbolTable())->AddSymbol(symproc); // Insert symproc to the proc's scope symbol table (for recursive call)
  if(!result) SetError(t, "duplicated symbol: this procedure and one of its parameter have same name");
  */

  // subroutineBody
  varDeclaration(proc);
  Consume(tKBegin);
  statseq = statSequence(proc);
  Consume(tKEnd);
  proc->SetStatementSequence(statseq);

  // procedure name matching
  Consume(tIdentifier, &t);
  if(t.GetValue().compare(proc->GetName()) != 0)
    SetError(t, "procedure name mismatch: expected " + proc->GetName() + ", got " + t.GetValue());

  Consume(tSemicolon);
}

CAstFunctionCall* CParser::subroutineCall(CAstScope *s, CToken t)
{
  //
  // subroutineCall ::= ident "(" [ expression { "," expression } ] ")".
  //
  // FIRST(subroutineCall) = { tIdentifier }
  // FOLLOW(subroutineCall) = { tKElse, tKEnd, tSemicolon, tMulDiv, tAnd, tPlusMinus,
  //                            tOr, tRelOp, tRBrak, tRParen, tComma }
  //
  // (FOLLOW(factor) = { tMulDiv, tAnd, | tPlusMinus, tOr | tRelOp |
  //                    tRBrak, tRParen, tComma, tKElse, tKEnd, tSemicolon } )
  //  
  //  Note: 'ident' is already consumed and passed as argument of 't'

  // Check if the symbol is present in this scope
  const CSymbol *sym = (s->GetSymbolTable())->FindSymbol(t.GetValue());
  if(sym == NULL)
    SetError(t, "procedure not found in this scope: " + t.GetValue());

  // Check if the symbol is of a function.
  const CSymProc *symproc = dynamic_cast<const CSymProc*>(sym);
  if(symproc == NULL)
    SetError(t, "not a procedure: " + t.GetValue());


  CAstFunctionCall *fc = new CAstFunctionCall(t, symproc);
  EToken tt;

  Consume(tLParen);
  while(!_abort) 
  {
    tt = _scanner->Peek().GetType();
    if(tt == tRParen) 
      break;
    else if (tt == tComma) 
      Consume(tComma);

    CAstExpression *e = expression(s);
    fc->AddArg(e);
  }
  Consume(tRParen);

  return fc;
}

CAstStatIf* CParser::ifStatement(CAstScope *s)
{
  //
  // ifStatement ::= "if" "(" expression ")" "then" statSequence [ "else" statSequence ] "end".
  //
  // FIRST(ifStatement) = { tKIf }
  // FOLLOW(ifStatement) = { tSemicolon, tKElse, tKEnd }
  
  CToken t;
  EToken tt;
  CAstExpression *cond = NULL;
  CAstStatement *ifBody = NULL, *elseBody = NULL;

  Consume(tKIf, &t);
  
  Consume(tLParen);
  cond = expression(s);
  Consume(tRParen);
  
  Consume(tKThen);
  ifBody = statSequence(s);
  
  tt = _scanner->Peek().GetType();
  if(tt == tKElse) 
  {
    Consume(tKElse);
    elseBody = statSequence(s);
  }

  Consume(tKEnd);

  return new CAstStatIf(t, cond, ifBody, elseBody);
}

CAstStatWhile* CParser::whileStatement(CAstScope *s)
{
  //
  // whileStatement ::= "while" "(" expression ")" "do" statSequence "end".
  //
  // FIRST(whileStatement) = { tKWhile }
  // FOLLOW(whileStatement) = { tSemicolon, tKElse, tKEnd }

  CToken t;
  CAstExpression *cond = NULL;
  CAstStatement *body = NULL;

  Consume(tKWhile, &t);
  
  Consume(tLParen);
  cond = expression(s);
  Consume(tRParen);

  Consume(tKDo);
  body = statSequence(s);
  Consume(tKEnd);

  return new CAstStatWhile(t, cond, body);
}

CAstStatReturn* CParser::returnStatement(CAstScope *s)
{
  //
  // returnStatement ::= "return" [ expression ].
  //
  // FIRST(returnStatement) = { tKReturn }
  // FOLLOW(returnStatement) = { tSemicolon, tKElse, tKEnd }
  
  CToken t;
  EToken tt;
  CAstExpression *ret_expr = NULL;

  Consume(tKReturn, &t);

  tt = _scanner->Peek().GetType();
  if(tt != tSemicolon && tt != tKElse && tt != tKEnd)
    ret_expr = expression(s);

  return new CAstStatReturn(t, s, ret_expr);
}

