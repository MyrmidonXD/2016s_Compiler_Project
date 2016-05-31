//------------------------------------------------------------------------------
/// @brief SnuPL abstract syntax tree
/// @author Bernhard Egger <bernhard@csap.snu.ac.kr>
/// @section changelog Change Log
/// 2012/09/14 Bernhard Egger created
/// 2013/05/22 Bernhard Egger reimplemented TAC generation
/// 2013/11/04 Bernhard Egger added typechecks for unary '+' operators
/// 2016/03/12 Bernhard Egger adapted to SnuPL/1
/// 2014/04/08 Bernhard Egger assignment 2: AST for SnuPL/-1
///
/// @section license_section License
/// Copyright (c) 2012-2016 Bernhard Egger
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
#include <cassert>
#include <cstring>
#include <climits>

#include <typeinfo>

#include "ast.h"
using namespace std;


//------------------------------------------------------------------------------
// CAstNode
//
int CAstNode::_global_id = 0;

CAstNode::CAstNode(CToken token)
  : _token(token), _addr(NULL)
{
  _id = _global_id++;
}

CAstNode::~CAstNode(void)
{
  if (_addr != NULL) delete _addr;
}

int CAstNode::GetID(void) const
{
  return _id;
}

CToken CAstNode::GetToken(void) const
{
  return _token;
}

const CType* CAstNode::GetType(void) const
{
  return CTypeManager::Get()->GetNull();
}

string CAstNode::dotID(void) const
{
  ostringstream out;
  out << "node" << dec << _id;
  return out.str();
}

string CAstNode::dotAttr(void) const
{
  return " [label=\"" + dotID() + "\"]";
}

void CAstNode::toDot(ostream &out, int indent) const
{
  string ind(indent, ' ');

  out << ind << dotID() << dotAttr() << ";" << endl;
}

CTacAddr* CAstNode::GetTacAddr(void) const
{
  return _addr;
}

ostream& operator<<(ostream &out, const CAstNode &t)
{
  return t.print(out);
}

ostream& operator<<(ostream &out, const CAstNode *t)
{
  return t->print(out);
}

//------------------------------------------------------------------------------
// CAstScope
//
CAstScope::CAstScope(CToken t, const string name, CAstScope *parent)
  : CAstNode(t), _name(name), _symtab(NULL), _parent(parent), _statseq(NULL),
    _cb(NULL)
{
  if (_parent != NULL) _parent->AddChild(this);
}

CAstScope::~CAstScope(void)
{
  delete _symtab;
  delete _statseq;
  delete _cb;
}

const string CAstScope::GetName(void) const
{
  return _name;
}

CAstScope* CAstScope::GetParent(void) const
{
  return _parent;
}

size_t CAstScope::GetNumChildren(void) const
{
  return _children.size();
}

CAstScope* CAstScope::GetChild(size_t i) const
{
  assert(i < _children.size());
  return _children[i];
}

CSymtab* CAstScope::GetSymbolTable(void) const
{
  assert(_symtab != NULL);
  return _symtab;
}

void CAstScope::SetStatementSequence(CAstStatement *statseq)
{
  _statseq = statseq;
}

CAstStatement* CAstScope::GetStatementSequence(void) const
{
  return _statseq;
}

bool CAstScope::TypeCheck(CToken *t, string *msg) const
{
  bool result = true;

  try
  {
    CAstStatement *curr = _statseq;
    while(result && (curr != NULL))
    {
      result = curr->TypeCheck(t, msg);
      curr = curr->GetNext();
    }

    vector<CAstScope*>::const_iterator it = _children.begin();
    while(result && it != _children.end())
    {
      result = (*it)->TypeCheck(t, msg);
      if((*it)->GetType()->IsArray())
      {
        (*t) = (*it)->GetToken();
        (*msg) = "Array cannot be a return type in functions";
        result = false;
        break;
      }
      it++;
    }
  }
  catch (...)
  {
    result = false;
  }

  return result;
}

ostream& CAstScope::print(ostream &out, int indent) const
{
  string ind(indent, ' ');

  out << ind << "CAstScope: '" << _name << "'" << endl;
  out << ind << "  symbol table:" << endl;
  _symtab->print(out, indent+4);
  out << ind << "  statement list:" << endl;
  CAstStatement *s = GetStatementSequence();
  if (s != NULL) {
    do {
      s->print(out, indent+4);
      s = s->GetNext();
    } while (s != NULL);
  } else {
    out << ind << "    empty." << endl;
  }

  out << ind << "  nested scopes:" << endl;
  if (_children.size() > 0) {
    for (size_t i=0; i<_children.size(); i++) {
      _children[i]->print(out, indent+4);
    }
  } else {
    out << ind << "    empty." << endl;
  }
  out << ind << endl;

  return out;
}

void CAstScope::toDot(ostream &out, int indent) const
{
  string ind(indent, ' ');

  CAstNode::toDot(out, indent);

  CAstStatement *s = GetStatementSequence();
  if (s != NULL) {
    string prev = dotID();
    do {
      s->toDot(out, indent);
      out << ind << prev << " -> " << s->dotID() << " [style=dotted];" << endl;
      prev = s->dotID();
      s = s->GetNext();
    } while (s != NULL);
  }

  vector<CAstScope*>::const_iterator it = _children.begin();
  while (it != _children.end()) {
    CAstScope *s = *it++;
    s->toDot(out, indent);
    out << ind << dotID() << " -> " << s->dotID() << ";" << endl;
  }

}

CTacAddr* CAstScope::ToTac(CCodeBlock *cb)
{
  assert(cb != NULL);

  CAstStatement *s = _statseq;
  while(s != NULL)
  {
    CTacLabel *next = cb->CreateLabel();
    s->ToTac(cb, next);
    cb->AddInstr(next);
    s = s->GetNext();
  }

  cb->CleanupControlFlow();

  return NULL;
}

CCodeBlock* CAstScope::GetCodeBlock(void) const
{
  return _cb;
}

void CAstScope::SetSymbolTable(CSymtab *st)
{
  if (_symtab != NULL) delete _symtab;
  _symtab = st;
}

void CAstScope::AddChild(CAstScope *child)
{
  _children.push_back(child);
}


//------------------------------------------------------------------------------
// CAstModule
//
CAstModule::CAstModule(CToken t, const string name)
  : CAstScope(t, name, NULL)
{
  SetSymbolTable(new CSymtab());
}

CSymbol* CAstModule::CreateVar(const string ident, const CType *type)
{
  return new CSymGlobal(ident, type);
}

string CAstModule::dotAttr(void) const
{
  return " [label=\"m " + GetName() + "\",shape=box]";
}



//------------------------------------------------------------------------------
// CAstProcedure
//
CAstProcedure::CAstProcedure(CToken t, const string name,
                             CAstScope *parent, CSymProc *symbol)
  : CAstScope(t, name, parent), _symbol(symbol)
{
  assert(GetParent() != NULL);
  SetSymbolTable(new CSymtab(GetParent()->GetSymbolTable()));
  assert(_symbol != NULL);
}

CSymProc* CAstProcedure::GetSymbol(void) const
{
  return _symbol;
}

CSymbol* CAstProcedure::CreateVar(const string ident, const CType *type)
{
  return new CSymLocal(ident, type);
}

const CType* CAstProcedure::GetType(void) const
{
  return GetSymbol()->GetDataType();
}

string CAstProcedure::dotAttr(void) const
{
  return " [label=\"p/f " + GetName() + "\",shape=box]";
}


//------------------------------------------------------------------------------
// CAstType
//
CAstType::CAstType(CToken t, const CType *type)
  : CAstNode(t), _type(type)
{
  assert(type != NULL);
}

const CType* CAstType::GetType(void) const
{
  return _type;
}

ostream& CAstType::print(ostream &out, int indent) const
{
  string ind(indent, ' ');

  out << ind << "CAstType (" << _type << ")" << endl;
  return out;
}


//------------------------------------------------------------------------------
// CAstStatement
//
CAstStatement::CAstStatement(CToken token)
  : CAstNode(token), _next(NULL)
{
}

CAstStatement::~CAstStatement(void)
{
  delete _next;
}

void CAstStatement::SetNext(CAstStatement *next)
{
  _next = next;
}

CAstStatement* CAstStatement::GetNext(void) const
{
  return _next;
}

CTacAddr* CAstStatement::ToTac(CCodeBlock *cb, CTacLabel *next)
{
  return NULL;
}


//------------------------------------------------------------------------------
// CAstStatAssign
//
CAstStatAssign::CAstStatAssign(CToken t,
                               CAstDesignator *lhs, CAstExpression *rhs)
  : CAstStatement(t), _lhs(lhs), _rhs(rhs)
{
  assert(lhs != NULL);
  assert(rhs != NULL);
}

CAstDesignator* CAstStatAssign::GetLHS(void) const
{
  return _lhs;
}

CAstExpression* CAstStatAssign::GetRHS(void) const
{
  return _rhs;
}

bool CAstStatAssign::TypeCheck(CToken *t, string *msg) const
{
  /* todo list
   * 1. TypeCheck lhs, rhs
   * 2. lhs Scalar check
   * 3. lhs-rhs match check
   */

  // TypeCheck LHS, RHS
  bool result = false;
  result = _lhs->TypeCheck(t, msg);
  if(!result) return false;

  result = _rhs->TypeCheck(t, msg);
  if(!result) return false;

  // INVALID type check
  const CType *lt = _lhs->GetType(), *rt = _rhs->GetType();
  if(lt == NULL)
  {
    (*t) = _lhs->GetToken();
    (*msg) = "Type <INVALID> appeared in LHS of assignment.";
    return false;
  }
  else if(rt == NULL)
  {
    (*t) = _rhs->GetToken();
    (*msg) = "Type <INVALID> appeared in RHS of assignment.";
    return false;
  }

  // LHS Scalar type check
  if(!lt->IsScalar())
  {
    (*t) = _lhs->GetToken();
    (*msg) = "Cannot assign to compound type lhs.";
    return false;
  }

  // LHS-RHS match check
  if(!lt->Match(rt))
  {
    (*t) = GetToken();
    (*msg) = "LHS and RHS type mismatch in assignment.";
    return false;
  }

  return true;
}

const CType* CAstStatAssign::GetType(void) const
{
  return _lhs->GetType();
}

ostream& CAstStatAssign::print(ostream &out, int indent) const
{
  string ind(indent, ' ');

  out << ind << ":=" << " ";

  const CType *t = GetType();
  if (t != NULL) out << t; else out << "<INVALID>";

  out << endl;

  _lhs->print(out, indent+2);
  _rhs->print(out, indent+2);

  return out;
}

string CAstStatAssign::dotAttr(void) const
{
  return " [label=\":=\",shape=box]";
}

void CAstStatAssign::toDot(ostream &out, int indent) const
{
  string ind(indent, ' ');

  CAstNode::toDot(out, indent);

  _lhs->toDot(out, indent);
  out << ind << dotID() << "->" << _lhs->dotID() << ";" << endl;
  _rhs->toDot(out, indent);
  out << ind << dotID() << "->" << _rhs->dotID() << ";" << endl;
}

CTacAddr* CAstStatAssign::ToTac(CCodeBlock *cb, CTacLabel *next)
{
  CTacAddr *LHS = _lhs->ToTac(cb);
  CTacAddr *RHS = _rhs->ToTac(cb);
  cb->AddInstr(new CTacInstr(opAssign, LHS, RHS));

  cb->AddInstr(new CTacInstr(opGoto, next));
  
  return NULL;
}


//------------------------------------------------------------------------------
// CAstStatCall
//
CAstStatCall::CAstStatCall(CToken t, CAstFunctionCall *call)
  : CAstStatement(t), _call(call)
{
  assert(call != NULL);
}

CAstFunctionCall* CAstStatCall::GetCall(void) const
{
  return _call;
}

bool CAstStatCall::TypeCheck(CToken *t, string *msg) const
{
  return GetCall()->TypeCheck(t, msg);
}

ostream& CAstStatCall::print(ostream &out, int indent) const
{
  _call->print(out, indent);

  return out;
}

string CAstStatCall::dotID(void) const
{
  return _call->dotID();
}

string CAstStatCall::dotAttr(void) const
{
  return _call->dotAttr();
}

void CAstStatCall::toDot(ostream &out, int indent) const
{
  _call->toDot(out, indent);
}

CTacAddr* CAstStatCall::ToTac(CCodeBlock *cb, CTacLabel *next)
{
  _call->ToTac(cb);  

  cb->AddInstr(new CTacInstr(opGoto, next));

  return NULL;
}


//------------------------------------------------------------------------------
// CAstStatReturn
//
CAstStatReturn::CAstStatReturn(CToken t, CAstScope *scope, CAstExpression *expr)
  : CAstStatement(t), _scope(scope), _expr(expr)
{
  assert(scope != NULL);
}

CAstScope* CAstStatReturn::GetScope(void) const
{
  return _scope;
}

CAstExpression* CAstStatReturn::GetExpression(void) const
{
  return _expr;
}

bool CAstStatReturn::TypeCheck(CToken *t, string *msg) const
{
  const CType *st = GetScope()->GetType();
  CAstExpression *e = GetExpression();

  // Procedure - No e exists
  if(st->IsNull())
  {
    if(e != NULL)
    {
      (*t) = e->GetToken();
      (*msg) = "Procedures cannot have return expression";
      return false;
    }
  }
  // Function - e exists
  else
  {
    if(e == NULL)
    {
      (*t) = GetToken();
      (*msg) = "Functions must have return expression";
      return false;
    }
    
    if(!e->TypeCheck(t, msg)) return false;

    // return type match check

    if(!st->Match(e->GetType()))
    {
      (*t) = GetToken();
      (*msg) = "Type mismatch in return value";
      return false;
    }
  }

  return true;
}

const CType* CAstStatReturn::GetType(void) const
{
  const CType *t = NULL;

  if (GetExpression() != NULL) {
    t = GetExpression()->GetType();
  } else {
    t = CTypeManager::Get()->GetNull();
  }

  return t;
}

ostream& CAstStatReturn::print(ostream &out, int indent) const
{
  string ind(indent, ' ');

  out << ind << "return" << " ";

  const CType *t = GetType();
  if (t != NULL) out << t; else out << "<INVALID>";

  out << endl;

  if (_expr != NULL) _expr->print(out, indent+2);

  return out;
}

string CAstStatReturn::dotAttr(void) const
{
  return " [label=\"return\",shape=box]";
}

void CAstStatReturn::toDot(ostream &out, int indent) const
{
  string ind(indent, ' ');

  CAstNode::toDot(out, indent);

  if (_expr != NULL) {
    _expr->toDot(out, indent);
    out << ind << dotID() << "->" << _expr->dotID() << ";" << endl;
  }
}

CTacAddr* CAstStatReturn::ToTac(CCodeBlock *cb, CTacLabel *next)
{
  CTacAddr* retval = _expr->ToTac(cb);

  cb->AddInstr(new CTacInstr(opReturn, NULL, retval));

  cb->AddInstr(new CTacInstr(opGoto, next));

  return NULL;
}


//------------------------------------------------------------------------------
// CAstStatIf
//
CAstStatIf::CAstStatIf(CToken t, CAstExpression *cond,
                       CAstStatement *ifBody, CAstStatement *elseBody)
  : CAstStatement(t), _cond(cond), _ifBody(ifBody), _elseBody(elseBody)
{
  assert(cond != NULL);
}

CAstExpression* CAstStatIf::GetCondition(void) const
{
  return _cond;
}

CAstStatement* CAstStatIf::GetIfBody(void) const
{
  return _ifBody;
}

CAstStatement* CAstStatIf::GetElseBody(void) const
{
  return _elseBody;
}

bool CAstStatIf::TypeCheck(CToken *t, string *msg) const
{
  const CType *ct = _cond->GetType();
  if(!_cond->TypeCheck(t, msg)) return false; // TypeCheck of condition
  if(!ct->IsBoolean()) // If condition is not a boolean type
  {
    (*t) = _cond->GetToken();
    (*msg) = "Not a boolean value in the condition of if statement";
    return false;
  }

  // ifBody TypeCheck
  CAstStatement *ifst = _ifBody;
  while(ifst != NULL)
  {
    if(!ifst->TypeCheck(t, msg)) return false;
    ifst = ifst->GetNext();
  }
  
  // elseBody TypeCheck
  CAstStatement *elsest = _elseBody;
  while(elsest != NULL)
  {
    if(!elsest->TypeCheck(t, msg)) return false;
    elsest = elsest->GetNext();
  }

  return true;
}

ostream& CAstStatIf::print(ostream &out, int indent) const
{
  string ind(indent, ' ');

  out << ind << "if cond" << endl;
  _cond->print(out, indent+2);
  out << ind << "if-body" << endl;
  if (_ifBody != NULL) {
    CAstStatement *s = _ifBody;
    do {
      s->print(out, indent+2);
      s = s->GetNext();
    } while (s != NULL);
  } else out << ind << "  empty." << endl;
  out << ind << "else-body" << endl;
  if (_elseBody != NULL) {
    CAstStatement *s = _elseBody;
    do {
      s->print(out, indent+2);
      s = s->GetNext();
    } while (s != NULL);
  } else out << ind << "  empty." << endl;

  return out;
}

string CAstStatIf::dotAttr(void) const
{
  return " [label=\"if\",shape=box]";
}

void CAstStatIf::toDot(ostream &out, int indent) const
{
  string ind(indent, ' ');

  CAstNode::toDot(out, indent);

  _cond->toDot(out, indent);
  out << ind << dotID() << "->" << _cond->dotID() << ";" << endl;

  if (_ifBody != NULL) {
    CAstStatement *s = _ifBody;
    if (s != NULL) {
      string prev = dotID();
      do {
        s->toDot(out, indent);
        out << ind << prev << " -> " << s->dotID() << " [style=dotted];"
            << endl;
        prev = s->dotID();
        s = s->GetNext();
      } while (s != NULL);
    }
  }

  if (_elseBody != NULL) {
    CAstStatement *s = _elseBody;
    if (s != NULL) {
      string prev = dotID();
      do {
        s->toDot(out, indent);
        out << ind << prev << " -> " << s->dotID() << " [style=dotted];" 
            << endl;
        prev = s->dotID();
        s = s->GetNext();
      } while (s != NULL);
    }
  }
}

CTacAddr* CAstStatIf::ToTac(CCodeBlock *cb, CTacLabel *next)
{
  CTacLabel *trueBody = cb->CreateLabel();
  CTacLabel *falseBody = (_elseBody != NULL) ? cb->CreateLabel() : next;
  
  _cond->ToTac(cb, trueBody, falseBody);
  
  cb->AddInstr(trueBody);
  
  CAstStatement *ts = _ifBody;
  while (ts != NULL)
  {
    CTacLabel *true_next = cb->CreateLabel();
    ts->ToTac(cb, true_next);
    cb->AddInstr(true_next);
    ts = ts->GetNext();
  }
  cb->AddInstr(new CTacInstr(opGoto, next));

  if(_elseBody != NULL)
  {
    cb->AddInstr(falseBody);

    CAstStatement *fs = _elseBody;
    while (fs != NULL)
    {
      CTacLabel *false_next = cb->CreateLabel();
      fs->ToTac(cb, false_next);
      cb->AddInstr(false_next);
      fs = fs->GetNext();
    }
  }

  cb->AddInstr(new CTacInstr(opGoto, next));

  return NULL;
}


//------------------------------------------------------------------------------
// CAstStatWhile
//
CAstStatWhile::CAstStatWhile(CToken t,
                             CAstExpression *cond, CAstStatement *body)
  : CAstStatement(t), _cond(cond), _body(body)
{
  assert(cond != NULL);
}

CAstExpression* CAstStatWhile::GetCondition(void) const
{
  return _cond;
}

CAstStatement* CAstStatWhile::GetBody(void) const
{
  return _body;
}

bool CAstStatWhile::TypeCheck(CToken *t, string *msg) const
{
  const CType *ct = _cond->GetType();
  if(!_cond->TypeCheck(t, msg)) return false; // TypeCheck of condition
  if(!ct->IsBoolean()) // If condition is not a boolean type
  {
    (*t) = _cond->GetToken();
    (*msg) = "Not a boolean value in the condition of while statement";
    return false;
  }

  // while body TypeCheck
  CAstStatement *bodyst = _body;
  while(bodyst != NULL)
  {
    if(!bodyst->TypeCheck(t, msg)) return false;
    bodyst = bodyst->GetNext();
  }

  return true;
}

ostream& CAstStatWhile::print(ostream &out, int indent) const
{
  string ind(indent, ' ');

  out << ind << "while cond" << endl;
  _cond->print(out, indent+2);
  out << ind << "while-body" << endl;
  if (_body != NULL) {
    CAstStatement *s = _body;
    do {
      s->print(out, indent+2);
      s = s->GetNext();
    } while (s != NULL);
  }
  else out << ind << "  empty." << endl;

  return out;
}

string CAstStatWhile::dotAttr(void) const
{
  return " [label=\"while\",shape=box]";
}

void CAstStatWhile::toDot(ostream &out, int indent) const
{
  string ind(indent, ' ');

  CAstNode::toDot(out, indent);

  _cond->toDot(out, indent);
  out << ind << dotID() << "->" << _cond->dotID() << ";" << endl;

  if (_body != NULL) {
    CAstStatement *s = _body;
    if (s != NULL) {
      string prev = dotID();
      do {
        s->toDot(out, indent);
        out << ind << prev << " -> " << s->dotID() << " [style=dotted];"
            << endl;
        prev = s->dotID();
        s = s->GetNext();
      } while (s != NULL);
    }
  }
}

CTacAddr* CAstStatWhile::ToTac(CCodeBlock *cb, CTacLabel *next)
{
  CTacLabel *cond_check = cb->CreateLabel();
  CTacLabel *whileBody = cb->CreateLabel();

  cb->AddInstr(cond_check);

  _cond->ToTac(cb, whileBody, next);
  
  cb->AddInstr(whileBody);
  
  CAstStatement *s = _body;
  while (s != NULL)
  {
    CTacLabel *stmt_next = cb->CreateLabel();
    s->ToTac(cb, stmt_next);
    cb->AddInstr(stmt_next);
    s = s->GetNext();
  }
  cb->AddInstr(new CTacInstr(opGoto, cond_check));
  
  return NULL;
}


//------------------------------------------------------------------------------
// CAstExpression
//
CAstExpression::CAstExpression(CToken t)
  : CAstNode(t)
{
}

CTacAddr* CAstExpression::ToTac(CCodeBlock *cb)
{
  return NULL;
}

CTacAddr* CAstExpression::ToTac(CCodeBlock *cb,
                                CTacLabel *ltrue, CTacLabel *lfalse)
{
  return NULL;
}


//------------------------------------------------------------------------------
// CAstOperation
//
CAstOperation::CAstOperation(CToken t, EOperation oper)
  : CAstExpression(t), _oper(oper)
{
}

EOperation CAstOperation::GetOperation(void) const
{
  return _oper;
}


//------------------------------------------------------------------------------
// CAstBinaryOp
//
CAstBinaryOp::CAstBinaryOp(CToken t, EOperation oper,
                           CAstExpression *l,CAstExpression *r)
  : CAstOperation(t, oper), _left(l), _right(r)
{
  // these are the only binary operation we support for now
  assert((oper == opAdd)        || (oper == opSub)         ||
         (oper == opMul)        || (oper == opDiv)         ||
         (oper == opAnd)        || (oper == opOr)          ||
         (oper == opEqual)      || (oper == opNotEqual)    ||
         (oper == opLessThan)   || (oper == opLessEqual)   ||
         (oper == opBiggerThan) || (oper == opBiggerEqual)
        );
  assert(l != NULL);
  assert(r != NULL);
}

CAstExpression* CAstBinaryOp::GetLeft(void) const
{
  return _left;
}

CAstExpression* CAstBinaryOp::GetRight(void) const
{
  return _right;
}

bool CAstBinaryOp::TypeCheck(CToken *t, string *msg) const
{
  EOperation oper = GetOperation();
  CTypeManager *tm = CTypeManager::Get();

  // TypeCheck for each operands.
  bool operand_tc = _left->TypeCheck(t, msg);
  if(!operand_tc) return false;
  operand_tc = _right->TypeCheck(t, msg);
  if(!operand_tc) return false;

  const CType *lType = _left->GetType(), *rType = _right->GetType();

  // INVALID type check
  if(lType == NULL || rType == NULL)
  {
    (*t) = GetToken();
    (*msg) = "Type <INVALID> appeared in operands.";
    return false;
  }

  // Type matching
  if((oper == opAdd) || (oper == opSub) ||
     (oper == opMul) || (oper == opDiv))
  {
    if(lType->Match(tm->GetInt()) && lType->Match(rType))
      return true;
    else
    {
      (*t) = GetToken();
      (*msg) = "Non-integer type in operands for binary add/sub/mul/div.";
      return false;
    }
  }
  else if((oper == opAnd) || (oper == opOr))
  {
    if(lType->Match(tm->GetBool()) && lType->Match(rType))
      return true;
    else
    {
      (*t) = GetToken();
      (*msg) = "Non-boolean type in operands for binary and/or.";
      return false;
    }
  }
  else if((oper == opEqual) || (oper == opNotEqual))
  {
    if(   (lType->Match(tm->GetInt()) || lType->Match(tm->GetBool()) || lType->Match(tm->GetChar())) 
       && lType->Match(rType))
      return true;
    else
    {
      (*t) = GetToken();
      (*msg) = "Non-scalar type operand/operand type mismatch for binary equality(equal/not-equal).";
      return false;
    }
  }
  else
  {
    if(   (lType->Match(tm->GetInt()) || lType->Match(tm->GetChar()))
       && lType->Match(rType))
      return true;
    else
    {
      (*t) = GetToken();
      (*msg) = "Non-char or non-int type operand/operand type mismatch for binary comparison(l/le/g/ge).";
      return false;
    }
  }
}

const CType* CAstBinaryOp::GetType(void) const
{
  EOperation oper = GetOperation();
  if((oper == opAdd) || (oper == opSub) || 
     (oper == opMul) || (oper == opDiv))
  {
    return CTypeManager::Get()->GetInt();
  }
  else
  {
    return CTypeManager::Get()->GetBool();
  }
}

ostream& CAstBinaryOp::print(ostream &out, int indent) const
{
  string ind(indent, ' ');

  out << ind << GetOperation() << " ";

  const CType *t = GetType();
  if (t != NULL) out << t; else out << "<INVALID>";

  out << endl;

  _left->print(out, indent+2);
  _right->print(out, indent+2);

  return out;
}

string CAstBinaryOp::dotAttr(void) const
{
  ostringstream out;
  out << " [label=\"" << GetOperation() << "\",shape=box]";
  return out.str();
}

void CAstBinaryOp::toDot(ostream &out, int indent) const
{
  string ind(indent, ' ');

  CAstNode::toDot(out, indent);

  _left->toDot(out, indent);
  out << ind << dotID() << "->" << _left->dotID() << ";" << endl;
  _right->toDot(out, indent);
  out << ind << dotID() << "->" << _right->dotID() << ";" << endl;
}

CTacAddr* CAstBinaryOp::ToTac(CCodeBlock *cb)
{
  if(GetType()->IsBoolean())
  {
    CTacLabel *if_true = cb->CreateLabel();
    CTacLabel *if_false = cb->CreateLabel();
    CTacLabel *next = cb->CreateLabel();
    CTacAddr *result = cb->CreateTemp(GetType());

    ToTac(cb, if_true, if_false);

    cb->AddInstr(if_true);
    cb->AddInstr(new CTacInstr(opAssign, result, new CTacConst(1)));
    cb->AddInstr(new CTacInstr(opGoto, next));

    cb->AddInstr(if_false);
    cb->AddInstr(new CTacInstr(opAssign, result, new CTacConst(0)));

    cb->AddInstr(next);

    return result;
  }
  else
  {
    CTacAddr *loperand = _left->ToTac(cb);
    CTacAddr *roperand = _right->ToTac(cb);
    CTacAddr *res = cb->CreateTemp(GetType());
    
    cb->AddInstr(new CTacInstr(GetOperation(), res, loperand, roperand));
    return res;
  }
}

CTacAddr* CAstBinaryOp::ToTac(CCodeBlock *cb,
                              CTacLabel *ltrue, CTacLabel *lfalse)
{
  CTacLabel *test_b = cb->CreateLabel();

  if(GetOperation() == opAnd)
  {
    _left->ToTac(cb, test_b, lfalse);
    cb->AddInstr(test_b);
    _right->ToTac(cb, ltrue, lfalse);
    return NULL;
  }
  else if(GetOperation() == opOr)
  {
    _left->ToTac(cb, ltrue, test_b);
    cb->AddInstr(test_b);
    _right->ToTac(cb, ltrue, lfalse);
    return NULL;
  }
  else if(IsRelOp(GetOperation()))
  {
    cb->AddInstr(new CTacInstr(GetOperation(), ltrue, _left->ToTac(cb), _right->ToTac(cb)));
    cb->AddInstr(new CTacInstr(opGoto, lfalse));
    return NULL;
  }
  else
  {
    bool isBoolean = false;
    assert(isBoolean == true);
    return NULL;
  }
  
}


//------------------------------------------------------------------------------
// CAstUnaryOp
//
CAstUnaryOp::CAstUnaryOp(CToken t, EOperation oper, CAstExpression *e)
  : CAstOperation(t, oper), _operand(e)
{
  assert((oper == opNeg) || (oper == opPos) || (oper == opNot));
  assert(e != NULL);
}

CAstExpression* CAstUnaryOp::GetOperand(void) const
{
  return _operand;
}

bool CAstUnaryOp::TypeCheck(CToken *t, string *msg) const
{
  EOperation oper = GetOperation();
  CAstConstant *test_operand = dynamic_cast<CAstConstant*>(_operand);
  bool result = false;

  if((oper == opPos) || (oper == opNeg))
  {
    bool operand_tc;
    if(oper == opNeg && test_operand != NULL)
      operand_tc = test_operand->TypeCheck(t, msg, true);
    else
      operand_tc = _operand->TypeCheck(t, msg);

    if(!operand_tc) return false;

    const CType *oType = _operand->GetType();
    if(oType == NULL) 
    {
      (*t) = GetToken();
      (*msg) = "Type <INVALID> appeared in operand";
        return false;
    }
    result = oType->Match(CTypeManager::Get()->GetInt());

    if(!result)
    {
      (*t) = GetToken();
      (*msg) = "Operand Mismatch: Not an integer for unary pos/neg.";
    }
  }
  else if (oper == opNot)
  {
    bool operand_tc = _operand->TypeCheck(t, msg);
    if(!operand_tc) return false;

    const CType *oType = _operand->GetType();
    if(oType == NULL) 
    {
      (*t) = GetToken();
      (*msg) = "Type <INVALID> appeared in operand";
        return false;
    }
    result = oType->Match(CTypeManager::Get()->GetBool());
    if(!result)
    {
      (*t) = GetToken();
      (*msg) = "Operand Mismatch: Not a boolean for unary not.";
    }
  }

  return result;
}

const CType* CAstUnaryOp::GetType(void) const
{ 
  EOperation oper = GetOperation();
  if(oper == opNot)
    return CTypeManager::Get()->GetBool();
  else
    return CTypeManager::Get()->GetInt();
}

ostream& CAstUnaryOp::print(ostream &out, int indent) const
{
  string ind(indent, ' ');

  out << ind << GetOperation() << " ";

  const CType *t = GetType();
  if (t != NULL) out << t; else out << "<INVALID>";
  out << endl;

  _operand->print(out, indent+2);

  return out;
}

string CAstUnaryOp::dotAttr(void) const
{
  ostringstream out;
  out << " [label=\"" << GetOperation() << "\",shape=box]";
  return out.str();
}

void CAstUnaryOp::toDot(ostream &out, int indent) const
{
  string ind(indent, ' ');

  CAstNode::toDot(out, indent);

  _operand->toDot(out, indent);
  out << ind << dotID() << "->" << _operand->dotID() << ";" << endl;
}

CTacAddr* CAstUnaryOp::ToTac(CCodeBlock *cb)
{
  if(GetType()->IsBoolean())
  {
    CTacLabel *if_true = cb->CreateLabel("if_true");
    CTacLabel *if_false = cb->CreateLabel("if_false");
    CTacLabel *next = cb->CreateLabel();
    CTacAddr *result = cb->CreateTemp(GetType());

    ToTac(cb, if_true, if_false);

    cb->AddInstr(if_true);
    cb->AddInstr(new CTacInstr(opAssign, result, new CTacConst(1)));
    cb->AddInstr(new CTacInstr(opGoto, next));

    cb->AddInstr(if_false);
    cb->AddInstr(new CTacInstr(opAssign, result, new CTacConst(0)));

    cb->AddInstr(next);

    return result; 
  }
  else
  {
    CTacAddr *operandResult = _operand->ToTac(cb);
    CTacAddr *result = cb->CreateTemp(GetType());

    cb->AddInstr(new CTacInstr(GetOperation(), result, operandResult));
    return result;
  }
  
}

CTacAddr* CAstUnaryOp::ToTac(CCodeBlock *cb,
                             CTacLabel *ltrue, CTacLabel *lfalse)
{
  if(GetOperation() == opNot)
  {
    cb->AddInstr(new CTacInstr(opEqual, ltrue, _operand->ToTac(cb), new CTacConst(0)));
    cb->AddInstr(new CTacInstr(opGoto, lfalse));
  }
  else
  {
    bool isThisUnaryOPBoolean = false;
    assert(isThisUnaryOPBoolean == true);
  }
  
  return NULL;
}


//------------------------------------------------------------------------------
// CAstSpecialOp
//
CAstSpecialOp::CAstSpecialOp(CToken t, EOperation oper, CAstExpression *e,
                             const CType *type)
  : CAstOperation(t, oper), _operand(e), _type(type)
{
  assert((oper == opAddress) || (oper == opDeref) || (oper = opCast));
  assert(e != NULL);
  assert((oper != opCast)); // opCast is unused.
}

CAstExpression* CAstSpecialOp::GetOperand(void) const
{
  return _operand;
}

bool CAstSpecialOp::TypeCheck(CToken *t, string *msg) const
{
  EOperation oper = GetOperation();
  CTypeManager *tm = CTypeManager::Get();

  // TypeCheck for the operand.
  bool operand_tc = _operand->TypeCheck(t, msg);
  if(!operand_tc) return false;

  // INVALID type check for operand
  const CType* oType = _operand->GetType();
  if(oType == NULL)
  {
    (*t) = GetToken();
    (*msg) = "Type <INVALID> appeared in operand.";
    return false;
  }
  
  // Type matching for each operation
  if(oper == opAddress)
  {
    // If operand is an arbitrary array
    if(oType->IsArray())
      return true;
    else
    {
      (*t) = GetToken();
      (*msg) = "Non-array type in operand for addressing operation '&()'";
      return false;
    }
  }
  else if(oper == opDeref)
  {
    // If operand is a pointer to an arbitrary array
    if(oType->Match(tm->GetPointer(tm->GetArray(CArrayType::OPEN, tm->GetNull()))))
      return true;
    else
    {
      (*t) = GetToken();
      (*msg) = "Non-pointer to array type in operand for dereferencing operation '*()'";
      return false;
    }
  }
  else // oper == opCast
  {
    (*t) = GetToken();
    (*msg) = "opCast is not supported in SnuPL/1.";
    return false;
  } 
}

const CType* CAstSpecialOp::GetType(void) const
{
  EOperation oper = GetOperation();
  if(oper == opAddress)
  {
    // return CTypeManager::Get()->GetPointer(_operand->GetType());
    return _type;
  }
  else if(oper == opDeref)
  {
    // return (_operand->GetType())->GetBaseType();
    return _type;
  }
  else
  {
    return NULL;
  }
}

ostream& CAstSpecialOp::print(ostream &out, int indent) const
{
  string ind(indent, ' ');

  out << ind << GetOperation() << " ";

  const CType *t = GetType();
  if (t != NULL) out << t; else out << "<INVALID>";
  out << endl;

  _operand->print(out, indent+2);

  return out;
}

string CAstSpecialOp::dotAttr(void) const
{
  ostringstream out;
  out << " [label=\"" << GetOperation() << "\",shape=box]";
  return out.str();
}

void CAstSpecialOp::toDot(ostream &out, int indent) const
{
  string ind(indent, ' ');

  CAstNode::toDot(out, indent);

  _operand->toDot(out, indent);
  out << ind << dotID() << "->" << _operand->dotID() << ";" << endl;
}

CTacAddr* CAstSpecialOp::ToTac(CCodeBlock *cb)
{
  CTacAddr* result = cb->CreateTemp(GetType());
  cb->AddInstr(new CTacInstr(GetOperation(), result, _operand->ToTac(cb)));
  return result;
}


//------------------------------------------------------------------------------
// CAstFunctionCall
//
CAstFunctionCall::CAstFunctionCall(CToken t, const CSymProc *symbol)
  : CAstExpression(t), _symbol(symbol)
{
  assert(symbol != NULL);
}

const CSymProc* CAstFunctionCall::GetSymbol(void) const
{
  return _symbol;
}

void CAstFunctionCall::AddArg(CAstExpression *arg)
{
  // Implicit type conversion of array arguments that are themselves not pointers
  if(arg->GetType()->IsArray() == true)
      arg = new CAstSpecialOp(arg->GetToken(),    // CToken t 
                              opAddress,          // EOperation o
                              arg,                // CAstExpression *e
                              CTypeManager::Get()->GetPointer(arg->GetType())); // const CType *type
  _arg.push_back(arg);
}

int CAstFunctionCall::GetNArgs(void) const                                                                                                                                                                                                                                                                                                                                                                                        
{
  return (int)_arg.size();
}

CAstExpression* CAstFunctionCall::GetArg(int index) const
{
  assert((index >= 0) && (index < _arg.size()));
  return _arg[index];
}

bool CAstFunctionCall::TypeCheck(CToken *t, string *msg) const
{ 
  CTypeManager *tm = CTypeManager::Get();

  const CSymProc *symproc = GetSymbol();

  /* todo list
   *  0. argument-parameter number match
   *  1. argument type check
   *  2. argument-parameter type match
   */

  // arg-param number match
  int n_param = symproc->GetNParams();
  int n_args = _arg.size();
  if(n_param != n_args)
  { 
    (*t) = GetToken();
    (*msg) = "Arity mismatch for calling: " + (*t).GetValue();
    return false;
  }

  // argument type check + arg-param type matching
  for(int i = 0; i < n_args; i++)
  {
    const CAstExpression* arg = _arg[i];
    const CSymParam* param = symproc->GetParam(i);

    // argument TypeCheck
    bool arg_tc = arg->TypeCheck(t, msg);
    if(!arg_tc) return false;

    // INVALID type check
    const CType *at = arg->GetType(), *pt = param->GetDataType();
    if(at == NULL)
    {
      (*t) = arg->GetToken();
      (*msg) = "Type <INVALID> appeared in arguments.";
      return false;
    }
    else if(pt == NULL)
    {
      (*t) = GetToken();
      (*msg) = "Type <INVALID> appeared in formal parameters.";
      return false;
    }
    
    // arg-param type matching
    bool result = pt->Match(at);
    if(!result)
    {
      (*t) = arg->GetToken();
      (*msg) = "Argument type not matched with corresponding parameter.";
      return false;
    }
  }

  return true;
}

const CType* CAstFunctionCall::GetType(void) const
{
  return GetSymbol()->GetDataType();
}

ostream& CAstFunctionCall::print(ostream &out, int indent) const
{
  string ind(indent, ' ');

  out << ind << "call " << _symbol << " ";
  const CType *t = GetType();
  if (t != NULL) out << t; else out << "<INVALID>";
  out << endl;

  for (size_t i=0; i<_arg.size(); i++) {
    _arg[i]->print(out, indent+2);
  }

  return out;
}

string CAstFunctionCall::dotAttr(void) const
{
  ostringstream out;
  out << " [label=\"call " << _symbol->GetName() << "\",shape=box]";
  return out.str();
}

void CAstFunctionCall::toDot(ostream &out, int indent) const
{
  string ind(indent, ' ');

  CAstNode::toDot(out, indent);

  for (size_t i=0; i<_arg.size(); i++) {
    _arg[i]->toDot(out, indent);
    out << ind << dotID() << "->" << _arg[i]->dotID() << ";" << endl;
  }
}

CTacAddr* CAstFunctionCall::ToTac(CCodeBlock *cb)
{
  CTacAddr *callResult = (GetType()->IsNull()) ? NULL : cb->CreateTemp(GetType());
  CTacAddr *func = new CTacName(_symbol);
  int n_args = GetNArgs();
  for(int i = n_args - 1; i >= 0; i--)
  {
    cb->AddInstr(new CTacInstr(opParam, new CTacConst(i), GetArg(i)->ToTac(cb)));
  }

  cb->AddInstr(new CTacInstr(opCall, callResult, func));

  return callResult;
}

CTacAddr* CAstFunctionCall::ToTac(CCodeBlock *cb,
                                  CTacLabel *ltrue, CTacLabel *lfalse)
{
  if(GetType()->IsBoolean())
  {
    CTacAddr *callResult = cb->CreateTemp(GetType());
    CTacAddr *func = new CTacName(_symbol);
    int n_args = GetNArgs();
    for(int i = n_args - 1; i >= 0; i--)
    {
      cb->AddInstr(new CTacInstr(opParam, new CTacConst(i), GetArg(i)->ToTac(cb)));
    }

    cb->AddInstr(new CTacInstr(opCall, callResult, func));

    cb->AddInstr(new CTacInstr(opEqual, ltrue, callResult, new CTacConst(1)));
    cb->AddInstr(new CTacInstr(opGoto, lfalse));
  }
  else
  {
    bool isThisFunctionBoolean = false;
    assert(isThisFunctionBoolean == true);
  }
  return NULL;
}



//------------------------------------------------------------------------------
// CAstOperand
//
CAstOperand::CAstOperand(CToken t)
  : CAstExpression(t)
{
}


//------------------------------------------------------------------------------
// CAstDesignator
//
CAstDesignator::CAstDesignator(CToken t, const CSymbol *symbol)
  : CAstOperand(t), _symbol(symbol)
{
  assert(symbol != NULL);
}

const CSymbol* CAstDesignator::GetSymbol(void) const
{
  return _symbol;
}

bool CAstDesignator::TypeCheck(CToken *t, string *msg) const
{
  const CType *dt = _symbol->GetDataType();
  CTypeManager *tm = CTypeManager::Get();

  // INVALID type check
  if(dt == NULL)
  {
    (*t) = GetToken();
    (*msg) = "Designator of type <INVALID> appeared.";
    return false;
  }

  // Function type Check
  const CSymProc* symproc = dynamic_cast<const CSymProc*>(_symbol);
  if(symproc != NULL)
  {
    (*t) = GetToken();
    (*msg) = "Cannot use a function to a designator.";
    return false;
  }

  return true;
}

const CType* CAstDesignator::GetType(void) const
{
  return GetSymbol()->GetDataType();
}

ostream& CAstDesignator::print(ostream &out, int indent) const
{
  string ind(indent, ' ');

  out << ind << _symbol << " ";

  const CType *t = GetType();
  if (t != NULL) out << t; else out << "<INVALID>";

  out << endl;

  return out;
}

string CAstDesignator::dotAttr(void) const
{
  ostringstream out;
  out << " [label=\"" << _symbol->GetName();
  out << "\",shape=ellipse]";
  return out.str();
}

void CAstDesignator::toDot(ostream &out, int indent) const
{
  string ind(indent, ' ');

  CAstNode::toDot(out, indent);
}

CTacAddr* CAstDesignator::ToTac(CCodeBlock *cb)
{
  return new CTacName(_symbol);
}

CTacAddr* CAstDesignator::ToTac(CCodeBlock *cb,
                                CTacLabel *ltrue, CTacLabel *lfalse)
{
  if(GetType()->IsBoolean())
  {
    CTacAddr* desigResult = new CTacName(_symbol);

    cb->AddInstr(new CTacInstr(opEqual, ltrue, desigResult, new CTacConst(1)));
    cb->AddInstr(new CTacInstr(opGoto, lfalse));
  }
  else
  {
    bool isThisDesignatorBoolean = false;
    assert(isThisDesignatorBoolean == true);
  }
  return NULL;
}


//------------------------------------------------------------------------------
// CAstArrayDesignator
//
CAstArrayDesignator::CAstArrayDesignator(CToken t, const CSymbol *symbol)
  : CAstDesignator(t, symbol), _done(false), _offset(NULL)
{
}

void CAstArrayDesignator::AddIndex(CAstExpression *idx)
{
  assert(!_done);
  _idx.push_back(idx);
}

void CAstArrayDesignator::IndicesComplete(void)
{
  assert(!_done);
  _done = true;
}

int CAstArrayDesignator::GetNIndices(void) const
{
  return (int)_idx.size();
}

CAstExpression* CAstArrayDesignator::GetIndex(int index) const
{
  assert((index >= 0) && (index < _idx.size()));
  return _idx[index];
}

bool CAstArrayDesignator::TypeCheck(CToken *t, string *msg) const
{
  const CSymbol* sym = GetSymbol();
  CTypeManager *tm = CTypeManager::Get();
  const CType *desig_type = sym->GetDataType();

  // Pointer Unboxing
  const CPointerType *p_desig_type = dynamic_cast<const CPointerType*>(desig_type);
  if(p_desig_type != NULL)
  {
    desig_type = p_desig_type->GetBaseType();
  }

  // Function type Check
  const CSymProc* symproc = dynamic_cast<const CSymProc*>(sym);
  if(symproc != NULL)
  {
    (*t) = GetToken();
    (*msg) = "Cannot use a function to an array designator.";
    return false;
  }

  // Check whether the type of this designator is array.
  const CArrayType *array_desig_type = dynamic_cast<const CArrayType*>(desig_type);
  if(array_desig_type == NULL) // if this designator is not an array
  {
    (*t) = GetToken();
    (*msg) = "This array designator is not an array type.";
    return false;
  }

  // Check the number of indexing
  int n_indices = (int)_idx.size();
  if(n_indices > array_desig_type->GetNDim())
  {
    (*t) = GetToken();
    (*msg) = "Too many indexing in this array designator.";
    return false;
  }

  // Check the type of index expressions
  for(int i = 0; i < n_indices; i++)
  {
    CAstExpression *ie = GetIndex(i);

    // index TypeCheck
    bool index_tc = ie->TypeCheck(t, msg);
    if(!index_tc) return false;

    // Check whether index is an integer type
    const CType *ie_type = ie->GetType();
    if(!ie_type->IsInt())
    {
      (*t) = ie->GetToken();
      (*msg) = "Not an integer type expression in array indices.";
      return false;
    }
  }

  return true;
}


const CType* CAstArrayDesignator::GetType(void) const
{
  const CSymbol *sym = GetSymbol();
  int index_num = _idx.size();
  const CType *desig_type = sym->GetDataType();

  // Pointer Unboxing
  const CPointerType *p_desig_type = dynamic_cast<const CPointerType*>(desig_type);
  if(p_desig_type != NULL)
  {
    desig_type = p_desig_type->GetBaseType();
  }

  // Check whether the type of this designator is array.
  const CArrayType *array_desig_type = dynamic_cast<const CArrayType*>(desig_type);
  if(array_desig_type == NULL) // if this designator is not an array
  {
    return NULL;
  }

  // Type calculation with indexing level
  int i = 0;
  while(desig_type != NULL && i < index_num)
  {
    const CArrayType *array_desig_type = dynamic_cast<const CArrayType*>(desig_type);
    if(array_desig_type == NULL) // If desig_type is already a basetype but unprocessed indices exist.
    {
      desig_type = NULL;
      break;
    }
    desig_type = array_desig_type->GetInnerType();
    i++;
  }

  return desig_type;
}

ostream& CAstArrayDesignator::print(ostream &out, int indent) const
{
  string ind(indent, ' ');

  out << ind << _symbol << " ";

  const CType *t = GetType();
  if (t != NULL) out << t; else out << "<INVALID>";

  out << endl;

  for (size_t i=0; i<_idx.size(); i++) {
    _idx[i]->print(out, indent+2);
  }

  return out;
}

string CAstArrayDesignator::dotAttr(void) const
{
  ostringstream out;
  out << " [label=\"" << _symbol->GetName() << "[]\",shape=ellipse]";
  return out.str();
}

void CAstArrayDesignator::toDot(ostream &out, int indent) const
{
  string ind(indent, ' ');

  CAstNode::toDot(out, indent);

  for (size_t i=0; i<_idx.size(); i++) {
    _idx[i]->toDot(out, indent);
    out << ind << dotID() << "-> " << _idx[i]->dotID() << ";" << endl;
  }
}

CTacAddr* CAstArrayDesignator::ToTac(CCodeBlock *cb)
{
  const CType *sym_type = GetSymbol()->GetDataType();
  if(sym_type->IsPointer())
  {
    sym_type = dynamic_cast<const CPointerType*>(sym_type)->GetBaseType();
  }

  const CArrayType *array_type = dynamic_cast<const CArrayType*>(sym_type);
  assert(array_type != NULL);

  int array_dim = array_type->GetNDim();
  int index_num = GetNIndices();
  // Get symbols of DIM and DOFS
  const CSymtab *sym_table = cb->GetOwner()->GetSymbolTable();
  const CSymProc *DIM_func = dynamic_cast<const CSymProc*>(sym_table->FindSymbol("DIM"));
  const CSymProc *DOFS_func = dynamic_cast<const CSymProc*>(sym_table->FindSymbol("DOFS"));

  assert(DIM_func != NULL);
  assert(DOFS_func != NULL);

  CToken dummy;
  const CType *int_type = CTypeManager::Get()->GetInt();
  const CType *ptr_type = CTypeManager::Get()->GetPointer(array_type);
  CAstExpression *result = NULL;

  // Make AST Node representing array address ( '&(A)' )
  CAstExpression *array_addr = NULL;
  if(GetSymbol()->GetDataType()->IsPointer())
  {
    array_addr = new CAstDesignator(dummy, GetSymbol());
  }
  else
  {
    array_addr = new CAstSpecialOp(dummy, opAddress, new CAstDesignator(dummy, GetSymbol()), ptr_type);
  }
  assert(array_addr != NULL);

  // Make AST for index address calculation part
  for(int i = 0; i < array_dim; i++)
  {
    if(result != NULL)
    {
      CAstFunctionCall *dim_call = new CAstFunctionCall(dummy, DIM_func);
      dim_call->AddArg(array_addr);
      dim_call->AddArg(new CAstConstant(dummy, int_type, i+1LL));
      
      result = new CAstBinaryOp(dummy, opMul, result, dim_call);

      CAstExpression *curr_idx = (i < index_num) ? GetIndex(i) : new CAstConstant(dummy, int_type, 0LL);

      result = new CAstBinaryOp(dummy, opAdd, result, curr_idx);

    }
    else
    {
      result = GetIndex(i); // This for first index.
    }
  }

  // Multiply index by a size of an element.
  result = new CAstBinaryOp(dummy, 
                            opMul, 
                            result, 
                            new CAstConstant(dummy, int_type, (long long)(array_type->GetBaseType()->GetSize())));

  // Add Data Offset to an indexing address
  CAstFunctionCall *dofs_call = new CAstFunctionCall(dummy, DOFS_func);
  dofs_call->AddArg(array_addr);

  result = new CAstBinaryOp(dummy, opAdd, result, dofs_call);

  // Add array starting address
  result = new CAstBinaryOp(dummy, opAdd, array_addr, result);

  // Call ToTac and return result!
  return new CTacReference(dynamic_cast<CTacName*>(result->ToTac(cb))->GetSymbol(), _symbol);
}

CTacAddr* CAstArrayDesignator::ToTac(CCodeBlock *cb,
                                     CTacLabel *ltrue, CTacLabel *lfalse)
{
  if(GetType()->IsBoolean())
  {
    cb->AddInstr(new CTacInstr(opEqual, ltrue, ToTac(cb), new CTacConst(1)));
    cb->AddInstr(new CTacInstr(opGoto, lfalse));
  }
  else
  {
    bool isThisArrayDesignatorBoolean = false;
    assert(isThisArrayDesignatorBoolean == true);
  }


  return NULL;
}


//------------------------------------------------------------------------------
// CAstConstant
//
CAstConstant::CAstConstant(CToken t, const CType *type, long long value)
  : CAstOperand(t), _type(type), _value(value)
{
}

void CAstConstant::SetValue(long long value)
{
  _value = value;
}

long long CAstConstant::GetValue(void) const
{
  return _value;
}

string CAstConstant::GetValueStr(void) const
{
  ostringstream out;

  if (GetType() == CTypeManager::Get()->GetBool()) {
    out << (_value == 0 ? "false" : "true");
  } else {
    out << dec << _value;
  }

  return out.str();
}

bool CAstConstant::TypeCheck(CToken *t, string *msg) const
{
  if(_type->Match(CTypeManager::Get()->GetInt()))
  {
    if(_value <= INT_MAX)
    {
      return true;
    }
    else
    {
      (*t) = GetToken();
      (*msg) = "Integer constant out of range.";
      return false;
    }
  }
  else if(_type->Match(CTypeManager::Get()->GetBool()))
  {
    return true;
  }
  else if(_type->Match(CTypeManager::Get()->GetChar()))
  {
    if(_value >= 0 && _value < 256)
      return true;
    else
    {
      (*t) = GetToken();
      (*msg) = "Char constant not an ASCII character.";
      return false;
    }
  }
  else
  {
    (*t) = GetToken();
    (*msg) = "Invalid type for CAstConstant";
    return false;
  } 
}

bool CAstConstant::TypeCheck(CToken *t, string *msg, bool isNeg) const
{ 
  if(_type->Match(CTypeManager::Get()->GetInt()))
  {
    long long maximum = isNeg ? INT_MAX + 1LL : INT_MAX;
    if(_value <= maximum)
    {
      return true;
    }
    else
    {
      (*t) = GetToken();
      (*msg) = "Integer constant out of range.";
      return false;
    }
  }
  else
    return TypeCheck(t, msg);
}

const CType* CAstConstant::GetType(void) const
{
  return _type;
}

ostream& CAstConstant::print(ostream &out, int indent) const
{
  string ind(indent, ' ');

  out << ind << GetValueStr() << " ";

  const CType *t = GetType();
  if (t != NULL) out << t; else out << "<INVALID>";

  out << endl;

  return out;
}

string CAstConstant::dotAttr(void) const
{
  ostringstream out;
  out << " [label=\"" << GetValueStr() << "\",shape=ellipse]";
  return out.str();
}

CTacAddr* CAstConstant::ToTac(CCodeBlock *cb)
{
  return new CTacConst(_value);
}

CTacAddr* CAstConstant::ToTac(CCodeBlock *cb,
                                CTacLabel *ltrue, CTacLabel *lfalse)
{
  if(GetType()->IsBoolean())
  {
    if(_value == 1LL) cb->AddInstr(new CTacInstr(opGoto, ltrue));
    else cb->AddInstr(new CTacInstr(opGoto, lfalse));
  }
  else
  {
    return new CTacConst(_value);
  }
  return NULL;
}


//------------------------------------------------------------------------------
// CAstStringConstant
//
int CAstStringConstant::_idx = 0;

CAstStringConstant::CAstStringConstant(CToken t, const string value,
                                       CAstScope *s)
  : CAstOperand(t)
{
  CTypeManager *tm = CTypeManager::Get();

  _type = tm->GetArray(strlen(CToken::unescape(value).c_str())+1,
                       tm->GetChar());
  _value = new CDataInitString(value);

  ostringstream o;
  o << "_str_" << ++_idx;

  _sym = new CSymGlobal(o.str(), _type);
  _sym->SetData(_value);
  s->GetSymbolTable()->AddSymbol(_sym);
}

const string CAstStringConstant::GetValue(void) const
{
  return _value->GetData();
}

const string CAstStringConstant::GetValueStr(void) const
{
  return GetValue();
}

bool CAstStringConstant::TypeCheck(CToken *t, string *msg) const
{
  if(_type->IsArray() && (dynamic_cast<const CArrayType*>(_type)->GetInnerType())->IsChar())
    return true;
  else
  {
    (*t) = GetToken();
    (*msg) = "[Parsing Error]Wrong type for String Constant";
    return false;
  }
}

const CType* CAstStringConstant::GetType(void) const
{
  return _type;
}

ostream& CAstStringConstant::print(ostream &out, int indent) const
{
  string ind(indent, ' ');

  out << ind << '"' << GetValueStr() << '"' << " ";

  const CType *t = GetType();
  if (t != NULL) out << t; else out << "<INVALID>";

  out << endl;

  return out;
}

string CAstStringConstant::dotAttr(void) const
{
  ostringstream out;
  // the string is already escaped, but dot requires double escaping
  out << " [label=\"\\\"" << CToken::escape(GetValueStr())
      << "\\\"\",shape=ellipse]";
  return out.str();
}

CTacAddr* CAstStringConstant::ToTac(CCodeBlock *cb)
{
  return new CTacName(_sym);
}

CTacAddr* CAstStringConstant::ToTac(CCodeBlock *cb,
                                CTacLabel *ltrue, CTacLabel *lfalse)
{
  return new CTacName(_sym);
}


