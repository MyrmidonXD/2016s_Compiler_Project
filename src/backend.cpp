//------------------------------------------------------------------------------
/// @brief SnuPL backend
/// @author Bernhard Egger <bernhard@csap.snu.ac.kr>
/// @section changelog Change Log
/// 2012/11/28 Bernhard Egger created
/// 2013/06/09 Bernhard Egger adapted to SnuPL/0
/// 2016/04/04 Bernhard Egger adapted to SnuPL/1
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

#include <fstream>
#include <sstream>
#include <iomanip>
#include <cassert>
#include <cstdlib>

#include "backend.h"
using namespace std;


//------------------------------------------------------------------------------
// CBackend
//
CBackend::CBackend(ostream &out)
  : _out(out)
{
}

CBackend::~CBackend(void)
{
}

bool CBackend::Emit(CModule *m)
{
  assert(m != NULL);
  _m = m;

  if (!_out.good()) return false;

  bool res = true;

  try {
    EmitHeader();
    EmitCode();
    EmitData();
    EmitFooter();

    res = _out.good();
  } catch (...) {
    res = false;
  }

  return res;
}

void CBackend::EmitHeader(void)
{
}

void CBackend::EmitCode(void)
{
}

void CBackend::EmitData(void)
{
}

void CBackend::EmitFooter(void)
{
}


//------------------------------------------------------------------------------
// CBackendx86
//
CBackendx86::CBackendx86(ostream &out)
  : CBackend(out), _curr_scope(NULL)
{
  _ind = string(4, ' ');
}

CBackendx86::~CBackendx86(void)
{
}

void CBackendx86::EmitHeader(void)
{
  _out << "##################################################" << endl
       << "# " << _m->GetName() << endl
       << "#" << endl
       << endl;
}

void CBackendx86::EmitCode(void)
{
  _out << _ind << "#-----------------------------------------" << endl
       << _ind << "# text section" << endl
       << _ind << "#" << endl
       << _ind << ".text" << endl
       << _ind << ".align 4" << endl
       << endl
       << _ind << "# entry point and pre-defined functions" << endl
       << _ind << ".global main" << endl
       << _ind << ".extern DIM" << endl
       << _ind << ".extern DOFS" << endl
       << _ind << ".extern ReadInt" << endl
       << _ind << ".extern WriteInt" << endl
       << _ind << ".extern WriteStr" << endl
       << _ind << ".extern WriteChar" << endl
       << _ind << ".extern WriteLn" << endl
       << endl;

  // forall s in subscopes do
  //   EmitScope(s)
  // EmitScope(program)
  
  SetScope(_m);
  CScope *curr_scope = GetScope();
  const vector<CScope*> &subscope_list = curr_scope->GetSubscopes();

  if(subscope_list.size() != 0)
    for(vector<CScope*>::const_iterator it = subscope_list.begin(); it != subscope_list.end(); it++)
      EmitScope((*it));
  EmitScope(curr_scope);

  _out << _ind << "# end of text section" << endl
       << _ind << "#-----------------------------------------" << endl
       << endl;
}

void CBackendx86::EmitData(void)
{
  _out << _ind << "#-----------------------------------------" << endl
       << _ind << "# global data section" << endl
       << _ind << "#" << endl
       << _ind << ".data" << endl
       << _ind << ".align 4" << endl
       << endl;

  EmitGlobalData(_m);

  _out << _ind << "# end of global data section" << endl
       << _ind << "#-----------------------------------------" << endl
       << endl;
}

void CBackendx86::EmitFooter(void)
{
  _out << _ind << ".end" << endl
       << "##################################################" << endl;
}

void CBackendx86::SetScope(CScope *scope)
{
  _curr_scope = scope;
}

CScope* CBackendx86::GetScope(void) const
{
  return _curr_scope;
}

void CBackendx86::EmitScope(CScope *scope)
{
  assert(scope != NULL);

  string label;

  if (scope->GetParent() == NULL) label = "main";
  else label = scope->GetName();

  // label
  _out << _ind << "# scope " << scope->GetName() << endl
       << label << ":" << endl;

  // ComputeStackOffsets(scope)
  //
  // emit function prologue
  //
  // forall i in instructions do
  //   EmitInstruction(i)
  //
  // emit function epilogue
  
  SetScope(scope);
  
  int stack_size = ComputeStackOffsets(scope->GetSymbolTable(), 8, -12);
  ostringstream o;
  o << ((stack_size - 12 + 3) / 4) * 4;

  // Emit Func Prologue
  _out << endl;
  _out << _ind << "# prologue" << endl;

  EmitInstruction("pushl", "%ebp", "");                             // 1. saving ebp by pushing it onto the stack
  EmitInstruction("movl", "%esp, %ebp", "");                        // 2. set ebp to esp
  EmitInstruction("pushl", "%ebx", "save callee saved registers");  // 3. save callee-saved registers
  EmitInstruction("pushl", "%esi", "");
  EmitInstruction("pushl", "%edi", "");
  EmitInstruction("subl", "$" + o.str() + ", %esp", "make room for locals");  // 4. Generate space on the stack for locals and
                                                                              //    spilled variables by adjusting the stack pointer.

  if(stack_size > 28)                                                         // 5. Memset local stack area to zero
  {
    int stack_num = (stack_size - 12 + 3) / 4;

    _out << endl;
    EmitInstruction("cld", "", "memset local stack area to 0");
    EmitInstruction("xorl", "%eax, %eax", "");
    EmitInstruction("movl", Imm(stack_num)+", %ecx", "");
    EmitInstruction("movl", "%esp, %edi", "");
    EmitInstruction("rep", "stosl", "");
  }
  else if(stack_size > 12)
  {
    int stack_num = (stack_size - 12 + 3) / 4;
  
    _out << endl;
    EmitInstruction("xorl", "%eax, %eax", "memset local stack area to 0");
    for(int i = stack_num-1; i >= 0; i--)
    {
      ostringstream s_ofs;
      s_ofs << 4 * i;

      EmitInstruction("movl", "%eax, "+s_ofs.str()+"(%esp)", "");
    }
  }

  EmitLocalData(scope);                                                       // 6. Emit Local Data to be initialized.

  _out << endl;
  _out << _ind << "# function body" << endl;

  const CCodeBlock *cb = scope->GetCodeBlock();
  const list<CTacInstr*> ilist = cb->GetInstr();
  for(list<CTacInstr*>::const_iterator it = ilist.begin(); it != ilist.end(); it++)
    EmitInstruction((*it));

  // Emit Func Epilogue
  _out << endl;
  EmitInstruction(new CTacLabel("exit"));
  _out << _ind << "# epilogue" << endl;
  EmitInstruction("addl", "$" + o.str() + ", %esp", "remove locals");  // 1. Remove space on stack for locals and spilled variables
  EmitInstruction("popl", "%edi", "");                    // 2. Restore callee-saved registers
  EmitInstruction("popl", "%esi", "");
  EmitInstruction("popl", "%ebx", "");
  EmitInstruction("popl", "%ebp", "");                    // 3. Restore ebp
  EmitInstruction("ret", "", "");                         // 4. Issue the ret instruction

  _out << endl;
}

void CBackendx86::EmitGlobalData(CScope *scope)
{
  assert(scope != NULL);

  // emit the globals for the current scope
  CSymtab *st = scope->GetSymbolTable();
  assert(st != NULL);

  bool header = false;

  vector<CSymbol*> slist = st->GetSymbols();

  _out << dec;

  size_t size = 0;

  for (size_t i=0; i<slist.size(); i++) {
    CSymbol *s = slist[i];
    const CType *t = s->GetDataType();

    if (s->GetSymbolType() == stGlobal) {
      if (!header) {
        _out << _ind << "# scope: " << scope->GetName() << endl;
        header = true;
      }

      // insert alignment only when necessary
      if ((t->GetAlign() > 1) && (size % t->GetAlign() != 0)) {
        size += t->GetAlign() - size % t->GetAlign();
        _out << setw(4) << " " << ".align "
             << right << setw(3) << t->GetAlign() << endl;
      }

      _out << left << setw(36) << s->GetName() + ":" << "# " << t << endl;

      if (t->IsArray()) {
        const CArrayType *a = dynamic_cast<const CArrayType*>(t);
        assert(a != NULL);
        int dim = a->GetNDim();

        _out << setw(4) << " "
          << ".long " << right << setw(4) << dim << endl;

        for (int d=0; d<dim; d++) {
          assert(a != NULL);

          _out << setw(4) << " "
            << ".long " << right << setw(4) << a->GetNElem() << endl;

          a = dynamic_cast<const CArrayType*>(a->GetInnerType());
        }
      }

      const CDataInitializer *di = s->GetData();
      if (di != NULL) {
        const CDataInitString *sdi = dynamic_cast<const CDataInitString*>(di);
        assert(sdi != NULL);  // only support string data initializers for now

        _out << left << setw(4) << " "
          << ".asciz " << '"' << sdi->GetData() << '"' << endl;
      } else {
        _out  << left << setw(4) << " "
          << ".skip " << dec << right << setw(4) << t->GetDataSize()
          << endl;
      }

      size += t->GetSize();
    }
  }

  _out << endl;

  // emit globals in subscopes (necessary if we support static local variables)
  vector<CScope*>::const_iterator sit = scope->GetSubscopes().begin();
  while (sit != scope->GetSubscopes().end()) EmitGlobalData(*sit++);
}

void CBackendx86::EmitLocalData(CScope *scope)
{
  // Initialize meta data of local arrays on stack
  assert(scope != NULL);

  CSymtab *st = scope->GetSymbolTable();
  assert(st != NULL);
  vector<CSymbol*> slist = st->GetSymbols();

  for(vector<CSymbol*>::const_iterator it = slist.begin(); it != slist.end(); it++)
  {
    if((*it)->GetSymbolType() == stLocal)
    {
      const CType *t = (*it)->GetDataType();
      assert(t != NULL);
      if(t->IsArray())
      {
        const CArrayType *at = dynamic_cast<const CArrayType*>(t);
        assert(at != NULL);

        string baseReg = (*it)->GetBaseRegister();
        int ofs = (*it)->GetOffset();
        int dim = at->GetNDim();

        ostringstream o1;
        o1 << ofs;
        string s_ofs = o1.str();

        // Initialize dimention  data
        EmitInstruction("movl", Imm(dim)+", "+s_ofs+"("+baseReg+")", "Initialize meta data of "+(*it)->GetName());

        for (int d=0; d<dim; d++) 
        {
          assert(at != NULL);
          ofs += 4;

          ostringstream o3;
          o3 << ofs;
          s_ofs = o3.str();
    
          // Initialize element number of this dimension.
          EmitInstruction("movl", Imm(at->GetNElem())+", "+s_ofs+"("+baseReg+")", "");

          at = dynamic_cast<const CArrayType*>(at->GetInnerType());
        }
      }
    }
  }


}

void CBackendx86::EmitCodeBlock(CCodeBlock *cb)
{
  assert(cb != NULL);

  const list<CTacInstr*> &instr = cb->GetInstr();
  list<CTacInstr*>::const_iterator it = instr.begin();

  while (it != instr.end()) EmitInstruction(*it++);
}

void CBackendx86::EmitInstruction(CTacInstr *i)
{
  assert(i != NULL);

  ostringstream cmt;
  string mnm;
  cmt << i;

  EOperation op = i->GetOperation();

  switch (op) {
    // binary operators
    // dst = src1 op src2
    case opAdd:
      Load(i->GetSrc(1), "%eax", cmt.str());
      Load(i->GetSrc(2), "%ebx", "");
      EmitInstruction("addl", "%ebx, %eax", "");
      Store(i->GetDest(), 'a', "");
      break;
    case opSub:
      Load(i->GetSrc(1), "%eax", cmt.str());
      Load(i->GetSrc(2), "%ebx", "");
      EmitInstruction("subl", "%ebx, %eax", "");
      Store(i->GetDest(), 'a', "");
      break;
    case opMul:
      Load(i->GetSrc(1), "%eax", cmt.str());
      Load(i->GetSrc(2), "%ebx", "");
      EmitInstruction("imull", "%ebx", "");
      Store(i->GetDest(), 'a', "");
      break;
    case opDiv:
      Load(i->GetSrc(1), "%eax", cmt.str());
      Load(i->GetSrc(2), "%ebx", "");
      EmitInstruction("cdq"); // sign-extend for dividend
      EmitInstruction("idivl", "%ebx", "");
      Store(i->GetDest(), 'a', "");
      break;
    case opAnd:
      Load(i->GetSrc(1), "%al", cmt.str());
      Load(i->GetSrc(2), "%bl", "");
      EmitInstruction("andb", "%bl, %al", "");
      Store(i->GetDest(), 'a', "");
      break;
    case opOr:
      Load(i->GetSrc(1), "%al", cmt.str());
      Load(i->GetSrc(2), "%bl", "");
      EmitInstruction("orb", "%bl, %al", "");
      Store(i->GetDest(), 'a', "");
      break;
    // unary operators
    // dst = op src1
    case opNeg:
      Load(i->GetSrc(1), "%eax", cmt.str());
      EmitInstruction("negl", "%eax", "");
      Store(i->GetDest(), 'a', "");
      break;
    case opPos:
      break; // opPos doesn't need any instruction.
    case opNot:
      Load(i->GetSrc(1), "%al", cmt.str());
      EmitInstruction("notb", "%al", "");
      Store(i->GetDest(), 'a', "");
      break;

    // memory operations
    // dst = src1
    case opAssign:
      Load(i->GetSrc(1), "%eax", cmt.str());
      Store(i->GetDest(), 'a', "");
      break;

    // pointer operations
    // dst = &src1
    case opAddress:
      {
        assert(i->GetSrc(1) != NULL);
        
        EmitInstruction("leal", Operand(i->GetSrc(1))+", %eax", cmt.str());
        Store(i->GetDest(), 'a', "");
        break;
      }
    // dst = *src1
    case opDeref:
      // opDeref not generated for now
      EmitInstruction("# opDeref", "not implemented", cmt.str());
      break;

    // unconditional branching
    // goto dst
    case opGoto:
      {
        const CTacLabel *goto_lbl = dynamic_cast<const CTacLabel*>(i->GetDest());
        assert(goto_lbl != NULL);

        EmitInstruction("jmp", Label(goto_lbl), cmt.str());
        break;
      }

    // conditional branching
    // if src1 relOp src2 then goto dst
    case opEqual:
    case opNotEqual:
    case opLessThan:
    case opLessEqual:
    case opBiggerThan:
    case opBiggerEqual:
      Load(i->GetSrc(1), "%eax", cmt.str());
      Load(i->GetSrc(2), "%ebx", "");
      EmitInstruction("cmpl", "%ebx, %eax", "");
      
      { // Emit conditional jump instrction
        CTacLabel *dst_lbl = dynamic_cast<CTacLabel*>(i->GetDest());
        assert(dst_lbl != NULL);

        EmitInstruction("j"+Condition(op), Label(dst_lbl), "");
        break;
      }

    // function call-related operations
    case opParam:
      Load(i->GetSrc(1), "%eax", cmt.str());
      EmitInstruction("pushl", "%eax", "");
      break;
    case opCall:
      {
        const CTacName* func_name = dynamic_cast<const CTacName*>(i->GetSrc(1));
        assert(func_name != NULL);
        const CSymProc* func_sym = dynamic_cast<const CSymProc*>(func_name->GetSymbol());
        assert(func_sym != NULL);

        int param_num = func_sym->GetNParams();

        EmitInstruction("call", Operand(i->GetSrc(1)), cmt.str());
        
        if(param_num > 0)
          EmitInstruction("addl", Imm(4*param_num)+", %esp", "");
        
        if(i->GetDest() != NULL)
          Store(i->GetDest(), 'a', "");
      }
      break;
    case opReturn:
      if(i->GetSrc(1) != NULL)
      {
        Load(i->GetSrc(1), "%eax", cmt.str());
        cmt.str("");
      }
      EmitInstruction("jmp", Label("exit"), cmt.str());
      break;

    // special
    case opLabel:
      _out << Label(dynamic_cast<CTacLabel*>(i)) << ":" << endl;
      break;

    case opNop:
      EmitInstruction("nop", "", cmt.str());
      break;


    default:
      EmitInstruction("# ???", "not implemented", cmt.str());
  }
}

void CBackendx86::EmitInstruction(string mnemonic, string args, string comment)
{
  _out << left
       << _ind
       << setw(7) << mnemonic << " "
       << setw(23) << args;
  if (comment != "") _out << " # " << comment;
  _out << endl;
}

void CBackendx86::Load(CTacAddr *src, string dst, string comment)
{
  assert(src != NULL);

  string mnm = "mov";
  string mod = "l";

  // set operator modifier based on the operand size
  switch (OperandSize(src)) {
    case 1: mod = "zbl"; break;
    case 2: mod = "zwl"; break;
    case 4: mod = "l"; break;
  }

  // emit the load instruction
  EmitInstruction(mnm + mod, Operand(src) + ", " + dst, comment);
}

void CBackendx86::Store(CTac *dst, char src_base, string comment)
{
  assert(dst != NULL);

  string mnm = "mov";
  string mod = "l";
  string src = "%";

  // compose the source register name based on the operand size
  switch (OperandSize(dst)) {
    case 1: mod = "b"; src += string(1, src_base) + "l"; break;
    case 2: mod = "w"; src += string(1, src_base) + "x"; break;
    case 4: mod = "l"; src += "e" + string(1, src_base) + "x"; break;
  }

  // emit the store instruction
  EmitInstruction(mnm + mod, src + ", " + Operand(dst), comment);
}

string CBackendx86::Operand(const CTac *op)
{
  string operand;

  // return a string representing op
  // hint: take special care of references (op of type CTacReference)

  const CTacConst *constant = dynamic_cast<const CTacConst*>(op);
  if(constant != NULL)
  {
    operand = Imm(constant->GetValue());
    return operand;
  }

  const CTacName *variable = dynamic_cast<const CTacName*>(op);
  assert(variable != NULL);

  // Reference handling
  // Get the symbol's value and put that to operand (memory address)
  const CTacReference *ref_var = dynamic_cast<const CTacReference*>(variable);
  if(ref_var != NULL)
  {
    const CSymbol *sym = ref_var->GetSymbol();
    CTacName *derefed_var = new CTacName(sym);
    
    operand = "(%edi)";
    Load(derefed_var, "%edi", "");
    return operand;
  }

  // Non-reference handling
  const CSymbol *sym = variable->GetSymbol();
  if(sym->GetSymbolType() == stGlobal || sym->GetSymbolType() == stProcedure)
  {
    operand = sym->GetName();
  }
  else 
  {
    stringstream s_stream;

    s_stream << sym->GetOffset();
    s_stream << "(" << sym->GetBaseRegister() << ")"; // ex) 12(%ebp)
    
    operand = s_stream.str();
  }
  return operand;
}

string CBackendx86::Imm(int value) const
{
  ostringstream o;
  o << "$" << dec << value;
  return o.str();
}

string CBackendx86::Label(const CTacLabel* label) const
{
  CScope *cs = GetScope();
  assert(cs != NULL);

  ostringstream o;
  o << "l_" << cs->GetName() << "_" << label->GetLabel();
  return o.str();
  return "l_" + cs->GetName() + "_" + label->GetLabel();
}

string CBackendx86::Label(string label) const
{
  CScope *cs = GetScope();
  assert(cs != NULL);

  return "l_" + cs->GetName() + "_" + label;
}

string CBackendx86::Condition(EOperation cond) const
{
  switch (cond) {
    case opEqual:       return "e";
    case opNotEqual:    return "ne";
    case opLessThan:    return "l";
    case opLessEqual:   return "le";
    case opBiggerThan:  return "g";
    case opBiggerEqual: return "ge";
    default:            assert(false); break;
  }
}

int CBackendx86::OperandSize(CTac *t) const
{
  int size = 4;

  // compute the size for operand t of type CTacName
  // Hint: you need to take special care of references (incl. references to pointers!)
  //       and arrays. Compare your output to that of the reference implementation
  //       if you are not sure.
 
  const CTacReference *ref_operand = dynamic_cast<const CTacReference*>(t);
  if(ref_operand != NULL)
  {
    const CSymbol *deref_sym = ref_operand->GetDerefSymbol();

    if(deref_sym->GetDataType()->IsArray())
      size = dynamic_cast<const CArrayType*>(deref_sym->GetDataType())->GetBaseType()->GetSize();
    else
      size = deref_sym->GetDataType()->GetSize();
  }
  else
  {
    const CTacName *name_operand = dynamic_cast<const CTacName*>(t);
    if(name_operand == NULL)
    {
      const CTacConst *const_operand = dynamic_cast<const CTacConst*>(t);
      assert(const_operand != NULL);

      size = 4; // We cannot determine the type of CTacConst.
    }
    else
    {
      const CSymbol *sym = name_operand->GetSymbol();
      if(sym->GetDataType()->IsArray())
        size = 4;
      else
        size = sym->GetDataType()->GetSize();
    }
  }

  return size;
}

size_t CBackendx86::ComputeStackOffsets(CSymtab *symtab,
                                        int param_ofs,int local_ofs)
{
  assert(symtab != NULL);
  vector<CSymbol*> slist = symtab->GetSymbols();

  int size = 0;
  int param_cnt = 0;

  assert(local_ofs < 0);
  assert(param_ofs > 0);

  // foreach local symbol l in slist do
  //   compute aligned offset on stack and store in symbol l
  //   set base register to %ebp
  //
  // foreach parameter p in slist do
  //   compute offset on stack and store in symbol p
  //   set base register to %ebp
  //
  // align size
  //
  // dump stack frame to assembly file

  for(vector<CSymbol*>::const_iterator it = slist.begin(); it != slist.end(); it++)
  {
    if((*it)->GetSymbolType() == stParam) // parameter handling
    {
      CSymParam *curr_param = dynamic_cast<CSymParam*>((*it));
      assert(curr_param != NULL);

      curr_param->SetOffset(param_ofs + 4 * curr_param->GetIndex()); // Every i-th arguments located in '%ebp + (param_ofs + 4*i)'
      curr_param->SetBaseRegister("%ebp");

      param_cnt++;
    }
    else if((*it)->GetSymbolType() == stLocal) // local symbol handling
    {
      int align = (*it)->GetDataType()->GetAlign();
      
      local_ofs -= (*it)->GetDataType()->GetSize(); 
      
      if(abs(local_ofs) % align != 0)
      {
        local_ofs -= align + (local_ofs % align); // decrese local_ofs by padding size (for alignment)
        // Note: remainder of negative number gives the negated remainder of its absolute value.
        // ex) (-19) % 4 = -3. (=> 4 + ((-19) % 4) = 1)
      }
      (*it)->SetOffset(local_ofs);
      (*it)->SetBaseRegister("%ebp");
    } 
  }

  _out << _ind << "# " << "stack offsets:" << endl;
  for(vector<CSymbol*>::const_iterator it = slist.begin(); it != slist.end(); it++)
  {
    if((*it)->GetSymbolType() == stParam || (*it)->GetSymbolType() == stLocal)
    {
      _out << _ind << "# " << right << setw(6) << (*it)->GetOffset()
                           << "(" << (*it)->GetBaseRegister() << ")"
                           << setw(4) << (*it)->GetDataType()->GetSize() << "  ";
      (*it)->print(_out, 0);
      _out << endl;
    }
  }
  
  size = -local_ofs;
  return size;
}
