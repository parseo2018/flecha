#!/usr/bin/python3
# -*- coding: utf-8 -*-

import sys
import os
sys.path.insert(0, '../..')

from parser import Parser
from ast_representation.program import Program
from ast_representation.parameters import Parameters
from ast_representation.apply_atomic_expression import ApplyAtomicExpression
from ast_representation.case_branch import CaseBranch
from ast_representation.case_expression import CaseExpression
from ast_representation.definition import Definition
from ast_representation.atomic_expression import AtomicExpression
from ast_representation.lambda_expression import LambdaExpression
from ast_representation.let_expression import LetExpression

class Flecha(Parser):

    reserved = {
        'if': 'IF',
        'else': 'ELSE',
        'elif': 'ELIF',
        'then': 'THEN',
        'def': 'DEF',
        'case': 'CASE',
        'def': 'DEF',
        'let': 'LET',
        'in': 'IN'
        }

    #literals = ['\n']

    tokens = list(reserved.values()) + [
        'DEFEQ','SEMICOLON','LPAREN','RPAREN','LAMBDA','PIPE','ARROW',
        'LOWERID','UPPERID',
  		'NUMBER',
  		'STRING', 'CHAR',
        'AND','OR','NOT',
        'EQ','NE','GE','LE','GT','LT',
        'PLUS','MINUS','TIMES','DIV','MOD'
        ]

    t_DEFEQ 	= r'='
    t_SEMICOLON = r';'
    t_LPAREN 	= r'\('
    t_RPAREN 	= r'\)'
    t_LAMBDA 	= r'\\'
    t_PIPE 		= r'\|'
    t_ARROW 	= r'->'

    t_AND 		= r'&&'

    t_OR        = r'\|\|'

    t_NOT 		= r'!'

    t_EQ 		= r'=='
    t_NE 		= r'!='
    t_GE 		= r'>='
    t_LE 		= r'<='
    t_GT 		= r'>'
    t_LT 		= r'<'

    t_PLUS 		= r'\+'
    t_MINUS 	= r'-'
    t_TIMES 	= r'\*'
    t_DIV 		= r'/'
    t_MOD 		= r'%'

    def t_LOWERID(self, t):
        r'''[a-z][_a-zA-Z_0-9]*'''

        t.type = Flecha.reserved.get(t.value,'LOWERID')    # Chequeo de palabras reservadas

        return t

    def t_UPPERID(self, t):
        r'''[A-Z][_a-zA-Z_0-9]*'''

        t.type = Flecha.reserved.get(t.value, 'UPPERID')  # Chequeo de palabras reservadas
        return t

    def t_CHAR(self, t):
        r'''(\'[^\'\\]*(?:\\.[^\'\\]*)*\')'''

        t.value = t.value[1:-1]
        return t

    def t_STRING(self, t):
        r'''(\"[^\"\\]*(?:\\.[^\"\\]*)*\")'''

        t.value = t.value[1:-1]
        return t

    def t_NUMBER(self, t):
        r'''\d+'''

        try:
            t.value = int(t.value)
        except ValueError:
            print ('Integer value too large', t.value)
            t.value = 0
        return t

    t_ignore_COMMENT = r'--.*'
    t_ignore = ' \t\r'

    precedence = (
    	('left','SEMICOLON'),
        ('left','IF', 'CASE', 'LET', 'LAMBDA'),
        ('left','THEN', 'ELSE', 'ELIF'),
    	('left','OR'),
    	('left','AND'),
    	('right','NOT'),
        ('nonassoc','EQ','NE','GE','LE','GT','LT'),
    	('left','PLUS', 'MINUS'),
    	('left','TIMES'),
    	('left','DIV', 'MOD'),
    	('right', 'UMINUS'),
    )

    # ******************* Program *******************
    def p_program(self, p):
    	''' program : empty_program
    				| not_empty_program '''         
    	p[0] = p[1]

    def p_empty(self, p):
    	''' empty : '''
    	pass

    def p_empty_program(self, p):
    	''' empty_program : empty '''
    	p[0] = Program(children=[])

    def p_not_empty_program(self, p):
        ''' not_empty_program : program definition_with_params 
                              | program definition_without_params '''
        p[0] = p[1].push(p[2])

    # ******************* Definition *******************

    def p_definition_without_params(self, p):
        ''' definition_without_params : DEF LOWERID empty_params DEFEQ expression '''
        p[0] = Definition(children=[p[2]] + [p[5]])

    def p_definition_with_params(self, p):
        ''' definition_with_params : DEF LOWERID params DEFEQ expression '''
        p[0] = Definition(children=[p[2]] + [LambdaExpression(children=[p[3],p[5]])])

    # ******************* Params *******************

    def p_empty_params(self, p):
        ''' empty_params : '''
        p[0] = []

    def p_params(self, p):
        ''' params : empty_params 
                   | not_empty_params
        '''
        p[0] = p[1]

    def p_not_empty_params(self, p):
        ''' not_empty_params : LOWERID params '''
        p[0] = [p[1]] + p[2]

    # ******************* Expression *******************

    def p_expression(self, p):
        ''' expression : outer_expression
                       | secuence_expression '''           
        p[0] = p[1]

    # ******************* SecuenceExpression *******************

    def p_secuence_expression(self, p):
        ''' secuence_expression : outer_expression SEMICOLON expression '''
        p[0] = LetExpression(children=["_", p[1], p[3]])

    def p_outer_expression(self, p):
        ''' outer_expression : inner_expression
                             | case_expression
                             | if_expression
                             | lambda_expression
                             | let_expression '''
        p[0] = p[1]


    def p_if_expression(self, p):
        ''' if_expression : IF inner_expression THEN inner_expression branch_else '''        
        p[0] = CaseExpression(children=[p[2]] + [[CaseBranch(children = ["True"] + [[]] + [p[4]])]+[p[5]]])

    def p_branch_else(self, p):
        ''' branch_else : elif_expression
                        | else_expression '''
        p[0] = p[1]

    def p_elif_expression(self, p):
        '''elif_expression : ELIF inner_expression THEN inner_expression branch_else'''
        p[0] = CaseBranch(children = ["False"] + [[]] + [ CaseExpression(children=[p[2]] + [[CaseBranch(children = ["True"] + [[]] + [p[4]])]+[p[5]]]) ])

        CaseExpression(children=[p[2]] + [[CaseBranch(children = ["False"] + [[]] + [p[4]])]])

    def p_else_expression(self, p):
        '''else_expression : ELSE inner_expression'''
        p[0] = CaseBranch(children = ["False"] + [[]] + [p[2]])

    def p_case_expression(self, p):
        ''' case_expression : CASE inner_expression branches_case '''
        p[0] = CaseExpression(children = [p[2]] + [p[3]])

    def p_branches_case(self, p):
        '''branches_case : empty_branch
                         | non_empty_branch
        '''
        p[0] = p[1]

    def p_empty_branch(self, p):
        '''empty_branch : '''
        p[0] = []

    def p_non_empty_branch(self, p):
        '''non_empty_branch : branch_case branches_case '''
        p[0] = [p[1]] + p[2]

    def p_branch_case(self, p):
        '''branch_case : PIPE UPPERID params ARROW inner_expression '''
        p[0] = CaseBranch(children = [p[2]] + [p[3]] + [p[5]])

    def p_let_expression(self, p):    
        ''' let_expression : LET LOWERID params DEFEQ inner_expression IN outer_expression '''
        count_params = len(p[3])
        if count_params >= 1:
            lamdaParameter = LambdaExpression(children=[p[3], p[5]])
        elif count_params == 0:
            lamdaParameter = p[5]
        p[0] = LetExpression(children=[p[2], lamdaParameter, p[7]])

    def p_lambda_expression(self, p):    
        ''' lambda_expression : LAMBDA params ARROW outer_expression                  
        '''
        count_params = len(p[2])
        if count_params >= 1:
            p[0] = LambdaExpression(children=[p[2], p[4]])
        elif count_params == 0:
            p[0] = p[4]
        return p[0]

    def p_inner_expression(self, p):
        ''' inner_expression : apply_expression
                             | binary_expression
                             | unary_expression '''
        p[0] = p[1]

    def p_apply_expression(self, p):
        ''' apply_expression : atomic_expression
                             | apply_atomic_expression '''
        p[0] = p[1]

    def p_atomic_expression(self, p):
        ''' atomic_expression : non_paren_atomic
                              | paren_atomic '''
        p[0] = p[1]

    def p_non_paren_atomic(self, p):
        ''' non_paren_atomic : char_expression
                             | string_expression
                             | number_expression
                             | lower_id_expression
                             | upper_id_expression '''
        p[0] = p[1]

    def get_char_ord(self, char):
        if char != '':
            if char[0] == "\\":
                char = bytes(char, encoding='ascii')
                char = char.decode('unicode-escape')
            char = str(ord(char))
        return char

    def p_char_expression(self, p):
        ''' char_expression : CHAR
        '''
        p[1] = self.get_char_ord(p[1])

        p[0] = AtomicExpression("ExprChar", p[1])

    def p_string_expression(self, p):
        ''' string_expression : STRING
        '''
        if p[1]:
            index_char = 1
            next_char = p[1][0]
            if next_char == "\\":
                index_char = 2
                next_char = p[1][0:index_char]
            sub = ApplyAtomicExpression(children=[AtomicExpression("ExprConstructor", "Cons"), AtomicExpression("ExprChar", self.get_char_ord(next_char))])
            p[1] = p[1][index_char:]
            p[0] = ApplyAtomicExpression(children=[sub,self.p_string_expression(p)])
        else:
            p[0] = AtomicExpression("ExprConstructor", "Nil")
        return p[0]

    def p_number_expression(self, p):
        ''' number_expression : NUMBER
        '''    
        p[0] = AtomicExpression("ExprNumber", str(p[1]))

    def p_lower_id_expression(self, p):
        ''' lower_id_expression : LOWERID
        '''    
        p[0] = AtomicExpression("ExprVar", p[1])

    def p_upper_id_expression(self, p):
        ''' upper_id_expression : UPPERID
        '''    
        p[0] = AtomicExpression("ExprConstructor", p[1])

    def p_paren_atomic(self, p):
        ''' paren_atomic : LPAREN expression RPAREN'''
        p[0] = p[2]

    def p_apply_atomic_expression(self, p):
        ''' apply_atomic_expression : apply_expression atomic_expression '''
        p[0] = ApplyAtomicExpression(children=[p[1], p[2]])

    def p_binary_expression(self, p):
        ''' binary_expression : inner_expression AND inner_expression
                              | inner_expression OR inner_expression
                              | inner_expression EQ inner_expression
                              | inner_expression NE inner_expression
                              | inner_expression GE inner_expression
                              | inner_expression LE inner_expression
                              | inner_expression GT inner_expression
                              | inner_expression LT inner_expression
                              | inner_expression PLUS inner_expression
                              | inner_expression MINUS inner_expression
                              | inner_expression TIMES inner_expression
                              | inner_expression DIV inner_expression
                              | inner_expression MOD inner_expression '''
        typeBinaryOp = None
        if p[2] == '&&' : typeBinaryOp = "AND"
        elif p[2] == '||': typeBinaryOp = "OR"
        elif p[2] == '==': typeBinaryOp = "EQ"
        elif p[2] == '!=': typeBinaryOp = "NE"
        elif p[2] == '>=': typeBinaryOp = "GE"
        elif p[2] == '<=': typeBinaryOp = "LE"
        elif p[2] == '>': typeBinaryOp = "GT"
        elif p[2] == '<': typeBinaryOp = "LT"
        elif p[2] == '+': typeBinaryOp = "ADD"
        elif p[2] == '-': typeBinaryOp = "SUB"
        elif p[2] == '*': typeBinaryOp = "MUL"
        elif p[2] == '/': typeBinaryOp = "DIV"
        elif p[2] == '%': typeBinaryOp = "MOD"
        subExpr = ApplyAtomicExpression(children=[AtomicExpression("ExprVar", typeBinaryOp), p[1]])
        p[0] = ApplyAtomicExpression(children=[subExpr, p[3]])

    """
    #ply yacc me obliga a escribir el codigo explicitamente como esta arriba para poder resolver la asociatividad y precedencia expresada en 'precedence'.
    #si lo escribo como esta abajo por alguna razon no se da cuenta y la asociatividad y precedencia es la defecto.
    def p_binary_op(self, p):
        '''binary_op : and_op
                     | or_op
                     | eq_op
                     | ne_op
                     | ge_op
                     | le_op
                     | gt_op
                     | lt_op
                     | plus_op
                     | minus_op
                     | times_op
                     | div_op
                     | mod_op
                     '''
        p[0] = p[1]

    def p_and_op(self, p):
        '''and_op : AND'''
        p[0] = AtomicExpression("ExprVar", "AND")

    def p_or_op(self, p):
        '''or_op : OR'''
        p[0] = AtomicExpression("ExprVar", "OR")

    def p_eq_op(self, p):
        '''eq_op : EQ'''
        p[0] = AtomicExpression("ExprVar", "EQ")

    def p_ne_op(self, p):
        '''ne_op : NE'''
        p[0] = AtomicExpression("ExprVar", "NE")

    def p_ge_op(self, p):
        '''ge_op : GE'''
        p[0] = AtomicExpression("ExprVar", "GE")

    def p_le_op(self, p):
        '''le_op : LE'''
        p[0] = AtomicExpression("ExprVar", "LE")

    def p_gt_op(self, p):
        '''gt_op : GT'''
        p[0] = AtomicExpression("ExprVar", "GT")

    def p_lt_op(self, p):
        '''lt_op : LT'''
        p[0] = AtomicExpression("ExprVar", "LT")

    def p_plus_op(self, p):
        '''plus_op : PLUS'''
        p[0] = AtomicExpression("ExprVar", "ADD")

    def p_minus_op(self, p):
        '''minus_op : MINUS'''
        p[0] = AtomicExpression("ExprVar", "SUB")

    def p_times_op(self, p):
        '''times_op : TIMES'''
        p[0] = AtomicExpression("ExprVar", "MUL")

    def p_div_op(self, p):
        '''div_op : DIV'''
        p[0] = AtomicExpression("ExprVar", "DIV")

    def p_mod_op(self, p):
        '''mod_op : MOD'''
        p[0] = AtomicExpression("ExprVar", "MOD")
    """
    def p_unary_expression(self, p):
        ''' unary_expression : NOT inner_expression
                             | MINUS inner_expression %prec UMINUS '''
        typeUnaryOp = None
        if p[1] == '!' : typeUnaryOp = "NOT"
        elif p[1] == '-': typeUnaryOp = "UMINUS"
        p[0] = ApplyAtomicExpression(children=[AtomicExpression("ExprVar", typeUnaryOp), p[2]])

    """
    #ply yacc me obliga a escribir el codigo explicitamente como esta arriba para poder resolver la asociatividad y precedencia expresada en 'precedence'.
    #si lo escribo como esta abajo por alguna razon no se da cuenta y la asociatividad y precedencia es la defecto.
    def p_unary_op(self, p):
        '''unary_op : not_op
                    | uminus_op'''
        p[0] = p[1]

    def p_not_op(self, p):
        '''not_op : NOT'''
        p[0] = AtomicExpression("ExprVar", "NOT")

    def p_uminus_op(self, p):
        '''uminus_op : MINUS %prec UMINUS'''
        p[0] = AtomicExpression("ExprVar", "UMINUS")
    """

    def t_newline(self, t):
        r'''\n+'''
        t.lexer.lineno += t.value.count('\n')

    def t_error(self, t):
        print ("Illegal character '%s'" % t.value[0])
        t.lexer.skip(1)

    def p_error(self, p):
        print (p)
        if p:
            print ('Syntax error at token', p.type)

            # Just discard the token and tell the parser it's okay.

            self.yacc.errok()
        else:
            print ('Syntax error at EOF')

"""
data = \
    '''
	-- Numeros
def uno = 1
def dos =2--comentario
def tres= 3  -- otro comentario
def
cuatro=4--comentario
def cinco = 5 def seis = 6def siete = 7
  def
    ocho
      =
         8 def
nueve
=9
def cero=0
def cerocero=00
def cerocerocero=000
def def_=10
def ifthenelse=11
def p_r_u_e_b_a=1987654321
def camelCase=12
def x1 = 11
def x2 = 12

'''

data01 = \
        '''
    -- Variables
def a=a
def b=    foo
def c        =bar
def camelCase = camelCase_
def camelCase_ = camelCase__
def camelCase__ = camelCase___
def camelCase___ = a
def x1=    x2
def x2=x3
def x3 = x1
def z = abcdefghijklmnopqrstuvwxyz01234567899876543210zyxwvutsrqponmlkjABC_DE_baabcdefghijklmnopqrstuvwxyz01234567899876543210zyxwvutsrqponmlkjihgfedcbaabcdefghijklmnopqrstuvwxyz01234567899876543210zyxwvutsrqponmlkjihgfedcbaabcdefghiFGH_I_J_K_L_Mwxyz01234567899876543210zyxwvutsrqponmlkjihgfedcbaabcdefghijklmnopqrstuvwxyz01234567899876543210zyxwvutsrqponmlkjihgfedcbaabcdefghijklmnopqrstuvwxyz01234567899876543210zyxwvutsrqponmlkjihgfedcbaabcdefghijklmnopqrstuvwxyz01234567899876543210zyxwvutsrqponmlkjihgfedcbaabcdefghijklmnopqrstuvwxyz01234567899876543210zyxwvutsrqponmlkjihgfedcbaabcdefghijklmnopqrstuvwxyz01234567899876543210zyxwvutsrqponmlkjihgfedcbaabcdefghijklmnopqrstuvwxyz01234567899876543210zyxwvutsrqponmlkjihgfedcbaabcdefghijklmnopqrstuvwxyz01234567899876543210zyxwvutsrqponmlkjihgfedcbaabcdefghijklmnopqrstuvwxyz01234567899876543210zyxwvutsrqponmlkjihgfedcbaabcdefghijklmnopqrstuvwxyz01234567899876543210zyxwvutsrqponmlkjihgfedcbaabcdefghijklmnopqrstuvwxyz01234567899876543210zyxwvutsrqponmlkj
def abcdefghijklmnopqrst01234567899876543210zyxwvutsrqponmlkjABC_DE_baabcdefghijklmnopqrstuvwxyz01234567899876543210zyxwvutsrqponmlkjihgfedcbaabcdefghijklmnopqrstuvwxyz01234567899876543210zyxwvutsrqponmlkjihgfedcbaabcdefghiFGH_I_J_K_L_Mwxyz01234567899876543210zyxwvutsrqponmlkjihgfedcbaabcdefghijklmnopqrstuvwxyz01234567899876543210zyxwvutsrqponmlkjihgfedcbaabcdefghijklmnopqrstuvwxyz01234567899876543210zyxwvutsrqponmlkjihgfedcbaabcdefghijklmnopqrstuvwxyz01234567899876543210zyxwvutsrqponmlkjihgfedcbaabcdefghijklmnopqrstuvwxyz01234567899876543210zyxwvutsrqponmlkjihgfedcbaabcdefghijklmnopqrstuvwxyz01234567899876543210zyxwvutsrqponmlkjihgfedcbaabcdefghijklmnopqrstuvwxyz01234567899876543210zyxwvutsrqponmlkjihgfedcbaabcdefghijklmnopqrstuvwxyz01234567899876543210zyxwvutsrqponmlkjihgfedcbaabcdefghijklmnopqrstuvwxyz01234567899876543210zyxwvutsrqponmlkjihgfedcbaabcdefghijklmnopqrstuvwxyz01234567899876543210zyxwvutsrqponmlkjihgfedcbaabcdefghijklmnopqrstuvwxyz01234567899876543210zyxwvutsrqponmlkj = y
'''

data02 = '''
    def a = 'a' def z = 'z' def a_ = 'A' def z_ = 'z'
def cero = '0' def nueve = '9' def espacio = ' ' def tab = '\t'
def cr = '\r' def lf = '\n' def comilla = '\'' def doble_comilla = '\"'
def contrabarra = '\\' def igual = '=' def lparen = '(' def rparen = ')'
-- Caracteres

'''

#Queda pendiente comillas y doble comillas
#def comilla = '\'' def doble_comilla = '\"'


data03 = \
'''
-- Constructores
def a=A
def b=    Foo
def c        =Bar
def camelCase = CamelCase_
def camelCase_ = CamelCase__
def camelCase__ = CamelCase___
def camelCase___ = A
def x1=    X2
def x2=X3
def x3 = X1
def z = Abcdefghijklmnopqrstuvwxyz01234567899876543210zyxwvutsrqponmlkjABC_DE_baabcdefghijklmnopqrstuvwxyz01234567899876543210zyxwvutsrqponmlkjihgfedcbaabcdefghijklmnopqrstuvwxyz01234567899876543210zyxwvutsrqponmlkjihgfedcbaabcdefghiFGH_I_J_K_L_Mwxyz01234567899876543210zyxwvutsrqponmlkjihgfedcbaabcdefghijklmnopqrstuvwxyz01234567899876543210zyxwvutsrqponmlkjihgfedcbaabcdefghijklmnopqrstuvwxyz01234567899876543210zyxwvutsrqponmlkjihgfedcbaabcdefghijklmnopqrstuvwxyz01234567899876543210zyxwvutsrqponmlkjihgfedcbaabcdefghijklmnopqrstuvwxyz01234567899876543210zyxwvutsrqponmlkjihgfedcbaabcdefghijklmnopqrstuvwxyz01234567899876543210zyxwvutsrqponmlkjihgfedcbaabcdefghijklmnopqrstuvwxyz01234567899876543210zyxwvutsrqponmlkjihgfedcbaabcdefghijklmnopqrstuvwxyz01234567899876543210zyxwvutsrqponmlkjihgfedcbaabcdefghijklmnopqrstuvwxyz01234567899876543210zyxwvutsrqponmlkjihgfedcbaabcdefghijklmnopqrstuvwxyz01234567899876543210zyxwvutsrqponmlkjihgfedcbaabcdefghijklmnopqrstuvwxyz01234567899876543210zyxwvutsrqponmlkj
def abcdefghijklmnopqrstuvwxyz01234567899876543210zyxwvutsrqponmlkjABC_DE_baabcdefghijklmnopqrstuvwxyz01234567899876543210zyxwvutsrqponmlkjihgfedcbaabcdefghijklmnopqrstuvwxyz01234567899876543210zyxwvutsrqponmlkjihgfedcbaabcdefghiFGH_I_J_K_L_Mwxyz01234567899876543210zyxwvutsrqponmlkjihgfedcbaabcdefghijklmnopqrstuvwxyz01234567899876543210zyxwvutsrqponmlkjihgfedcbaabcdefghijklmnopqrstuvwxyz01234567899876543210zyxwvutsrqponmlkjihgfedcbaabcdefghijklmnopqrstuvwxyz01234567899876543210zyxwvutsrqponmlkjihgfedcbaabcdefghijklmnopqrstuvwxyz01234567899876543210zyxwvutsrqponmlkjihgfedcbaabcdefghijklmnopqrstuvwxyz01234567899876543210zyxwvutsrqponmlkjihgfedcbaabcdefghijklmnopqrstuvwxyz01234567899876543210zyxwvutsrqponmlkjihgfedcbaabcdefghijklmnopqrstuvwxyz01234567899876543210zyxwvutsrqponmlkjihgfedcbaabcdefghijklmnopqrstuvwxyz01234567899876543210zyxwvutsrqponmlkjihgfedcbaabcdefghijklmnopqrstuvwxyz01234567899876543210zyxwvutsrqponmlkjihgfedcbaabcdefghijklmnopqrstuvwxyz01234567899876543210zyxwvutsrqponmlkj = Y
'''

data04 = \
'''
-- Estructuras
def lista_vacia = Nil
def lista1      = Cons 1 lista_vacia
def lista123    = Cons 1 (Cons 2 (Cons 3 Nil))
def listaABC    = Cons 'a' (Cons 'b' (Cons 'c' Nil))
def listaStrings =
  Cons "" (Cons "a" (Cons "ab" (Cons "abc" Nil)))
def arbolBinario =
  Bin x
      (Bin y Nil Nil)
      (Bin z Nil Nil)
def natural = S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S O)))))))))))))))))))
'''
#def lista123    = Cons 1 (Cons 2 (Cons 3 Nil))

data05= '''
-- Strings
def vacio = ""
def a = "a"
def abc = "abc"
def z = "z"
def a_ = "A"
def z_ = "Z"
def cero = "0"
def cero_cero = "00"
def cero_cero_cero = "000"
def nueve = "9"
def espacio = " "
def tab = "\t"
def cr = "\r"
def lf = "\n"
def comilla = "\'"
def doble_comilla = "\""
def contrabarra = "\\"
def contrabarra_n = "\\n"
def igual = "="
def lparen = "("
def rparen = ")"
def no_es_comentario = "hola -- esto no es un comentario"
def no_es_definicion = "def basura = 7 -- no es parte del programa"
def hola_mundo = "hola mundo\n"
def hola_mundo_escapado = "print \"hola mundo\\n\""
def contrabarras = "\\\\\\\\"
def espacios = "                                "
def b_vacio = " "
def b_a = " a"
def b_abc = " abc"
def b_z = " z"
def b_a_ = " A"
def b_z_ = " Z"
def b_cero = " 0"
def b_cero_cero = " 00"
def b_cero_cero_cero = " 000"
def b_nueve = " 9"
def b_espacio = "  "
def b_tab = " \t"
def b_cr = " \r"
def b_lf = " \n"
def b_comilla = " \'"
def b_doble_comilla = " \""
def b_contrabarra = " \\"
def b_contrabarra_n = " \\n"
def b_igual = " ="
def b_lparen = " ("
def b_rparen = " )"
def c_vacio = "  "
def c_a = " a "
def c_abc = " abc "
def c_z = " z "
def c_a_ = " A "
def c_z_ = " Z "
def c_cero = " 0 "
def c_cero_cero = " 00 "
def c_cero_cero_cero = " 000 "
def c_nueve = " 9 "
def c_espacio = "   "
def c_tab = " \t "
def c_cr = " \r "
def c_lf = " \n "
def c_comilla = " \' "
def c_doble_comilla = " \" "
def c_contrabarra = " \\ "
def c_contrabarra_n = " \\n "
def c_igual = " = "
def c_lparen = " ( "
def c_rparen = " ) "
def larga = "abcdefghijklmnopqrstuvwxyz01234567899876543210zyxwvutsrqponmlkjihgfedcba""
'''

data06 = '''
-- Case

def t1 = case x -- Case vacio

def t2 = case x
         | True  -> a
         | False -> b

def t3 = case x
         | X1 -> a

def t4 = case x
         | X1 -> a
         | X2 -> b

def t5 = case x
         | X1 -> a
         | X2 -> b
         | X3 -> c

def t6 = case x
         | X1 -> a
         | X2 -> b
         | X3 -> c
         | X4 -> d

def t7 = case x
         | Tupla x1 -> x1

def t8 = case x
         | Tupla x1 x2 -> x2

def t9 = case x
         | Tupla x1 x2 x3 -> x3

def t10 = case x
          | Tupla x1 x2 x3 x4 -> x4

def t11 = case x
          | Nil       -> a
          | Bin x i d -> b

def t12 = case x
          | C1 x1 x2 x3 -> x3
          | C2 x1 x2 x3 -> x2
          | C3 x1 x2 x3 -> x1

def t13 = case x|C1 x1 x2 x3->x3|C2 x1 x2 x3->x2|C3 x1 x2 x3->x1

def t13 = case X
          | C1 x1 x2 x3 -> X1
          | C2 x1 x2 x3 -> X2
          | C3 x1 x2 x3 -> X3

def t14 = case 1
          | C1 x1 x2 x3 -> 2
          | C2 x1 x2 x3 -> 3
          | C3 x1 x2 x3 -> 4

def t15 = case 'A'
          | C1 x1 x2 x3 -> 'B'
          | C2 x1 x2 x3 -> 'C'
          | C3 x1 x2 x3 -> 'D'

def t16 = case "A"
          | C1 x1 x2 x3 -> "B"
          | C2 x1 x2 x3 -> "C"
          | C3 x1 x2 x3 -> "D"

def t17 = case x
          | C1 x1 x2 x3 -> (case y)
          | C2 x1 x2 x3 -> (case x | X1 -> a)
          | C3 x1 x2 x3 -> (case x
                            | X1 a -> 1
                            | X2 a b -> 'b'
                            | X3 a b c -> 'C'
                            | X4 a b c d -> "d")

'''

data07 = '''
-- If

def t1 = if x then y else z
def t2 = if x1 then y1 elif x2 then y2 else z
def t3 = if x1 then y1
         elif x2 then y2
         elif x3 then y3
         else z
def t4 = if x1 then y1
         elif x2 then y2
         elif x3 then y3
         elif x4 then y4
         else z
def t5 = if X1 then Y1
         elif X2 then Y2
         elif X3 then Y3
         elif X4 then Y4
         else Z
def t6 = if 1 then 2
         elif 3 then 4
         elif 5 then 6
         elif 7 then 8
         else 9
def t7 = if 'a' then 'b'
         elif 'c' then 'd'
         elif 'e' then 'f'
         elif 'g' then 'h'
         else 'i'
def t8 = if "X1" then "Y1"
         elif "X2" then "Y2"
         elif "X3" then "Y3"
         elif "X4" then "Y4"
         else "Z"
def t9 = if (if x1 then 1 else X1)
         then (if x2 then 2 else '2')
         elif (if x3 then 3 else "3")
         then (if x4 then 4 else X4)
         elif (if x5 then 5 else '5')
         then (if x6 then 6 else "6")
         elif (if x7 then 7 else X7)
         then (if x8 then 8 else '8')
         else (if x9 then 9 else "9")
def t10 = if (if (if x then y else z) then y else z) then y else z
def t11 = if x then (if x then (if x then y else z) else z) else z
def t12 = if x then y else (if x then y else (if x then y else z))

'''

data08 = '''
-- Aplicacion

def t1 = a b
def t2 = a b c
def t3 = a b c d
def t4 = A b
def t5 = A b c
def t6 = A b c d
def t6 = a B
def t7 = a B C
def t8 = a B C D
def t9 = A B
def t10 = A B C
def t11 = A B C D
def t12 = a (b (c d e f) (g h i j) (k l m n))
            (o (p q r s) (t u v w) (x y z A))
            (B (C D E F) (G H I J) (K L M N))
            (O (P Q R S) (T U V W) (X Y Z))
def t13=a(b(c d e f)(g h i j)(k l m n))(o(p q r s)(t u v w)(x y
z A))(B(C D E F)(G H I J)(K L M N))(O(P Q R S)(T U V W)(X Y Z))
def t14=((((((((((((((((((((a))))))))))))))))))))
def t15=f 1 2 3 4 5 6 7 8 9 10
def t15=((f 1) (2) (3) 4) 5 6 7 8 9 10
def t15=f 'a' 'b' 'c' 'd' 'e' 'f' 'g' 'h' 'i' 'l'
def t15=f (g 'a' 'b' 1 2) (g 'a' 'b' 1 2) (g 'a' 'b' 1 2) (g 'a' 'b' 1 2)
def t16= Bin 1
           (Bin 2
             (Bin 3 Nil Nil)
             (Bin 4 Nil Nil))
           (Bin 5
             (Bin 6 Nil Nil)
             (Bin 7 Nil Nil))
'''

data09 = '''
-- Funciones anonimas

def t1 = \\ x -> x
def t2=\\x->x
def t3=\\x->
--
x
def t4 = \\ x y -> y
def t5 = \\ x y z -> z
def t6 = \\ x -> \\ y z -> z
def t7 = \\ x y z -> \\ a b c -> \\ d e f -> d
def t8 = \\ x y z -> \\ a b c -> \\ d e f -> F
def t9 = \\ x y z -> \\ a b c -> \\ d e f -> 1
def t10 = \\ x y z -> \\ a b c -> \\ d e f -> 'a'
def t11 = \\ x y z -> \\ a b c ->
          \\ x1 y1 z1 -> \\ a1 b1 c1 ->
            "aa"
def t12 = \\ x -> x y z
def t13 = (\\ x -> x y z)
def t14 = (\\ x -> x y) z
def t15 = (\\ x -> x) y z
def t16 = \\ x -> x y (z)
def t17 = \\ x -> x (y z)
def t18 = \\ -> v -- lambda vacia
def t19 = \\ x y z -> a b c


'''

data10 = '''
-- Declaraciones locales

def t1 = let x = y in z
def t2 = let x=y in z
def t3 = let x1 = y1 in
         let x2 = y2 in
         let x3 = y3
           in z
def t4 = let f x = x in f a
def t5 = let f x y = y in f a b
def t6 = let f x y z = z in f a b c
def t7 = let f x y z = z in
         let g h = h a b c in
           g f
def t8 = let f x y z = A in
         let g h = B in
         let x = C in
           D
def t8 = let f x y z = 1 in
         let g h = 2 in
         let x = 3 in
           4
def t9 = let f x y z = 'a' in
         let g h = 'b' in
         let x = 'c' in
           'd'
def t10 = let f x y z = "a" in
          let g h = "b" in
          let x = "c" in
            "d"
def t11 = let f x y z = (let x = y in z) in
          let g h =
               (let x1 = y1 in
                        let x2 = y2 in
                        let x3 = y3
                          in z) in
          let x = (let f x y z = z in
                   let g h = h a b c in
                       g f) in
            "d"

'''

data11 = '''
-- Secuenciación

def t1 = a ; b

def t2 = a ; b ; c

def t3 = (a ; b) ; c

def t4 = a ; (b ; c)

def t5 = a1;a2;a3;a4;a5;a6;a7;a8;a9;a10

def t6 = 1 ; 'a' ; A ; a

def t7 = print "a\n" ;
         print "b\n" ;
         print "c\n"

def t8 = if x1 then y1 else z1 ; 
         if x2 then y2 else z2 ;
         if x3 then y3 else z3

def t9 = case x1 ; 
         case x2 | A1 -> a1 | B1 -> b2 ; 
         case x3 | B1 -> b1 | B2 -> b2 | B3 -> b3

def t10 = let x1 = y1 in z1;let x2 = y2 in z2 ;
          let x3 = y3 in z3

def t11 = \\ x1 -> y1 z1 ;
          \\ x2 -> y2 z2 ;
          \\ x3 -> y3 z3

def t12 = a b (c d ; e f) ; g h i ; j k l
'''

data12 = '''
-- Anidamiento de estructuras de control

-- If/case

def t1 = if (case x | A -> a | B -> b) then (case y | A x -> a | B -> b) else (case z | A -> a | B x y z -> b)

-- If/let

def t2 = if (let x = 1 in 2) then (let y w = 1 in 2) else (let z a b c = 1 in 2)

-- If/lambda

def t3 = if (\\ x -> x) then (\\ y1 y2 y3 -> y1) else (\\ z -> z)

-- If/secuencia

def t4 = if (a ; b) then (c ; d) else (e ; f ; g)

-- Elif/case

def t5 = if a then b elif (case c | A -> a | B -> b) then (case d | A -> a | B -> b) else e

-- Elif/let

def t6 = if a then b elif (let x = 'a' in x) then (let x = 'b' in x) else e

-- Elif/lambda

def t7 = if a then b elif (\\ c1 c2 -> c2 c1) then (\\ d1 d2 d3 -> C d1 d2 d3) else e

-- Elif/secuencia

def t8 = if a then b elif (a ; b) then (d ; e) else e

-- Case/if

def t9 = case (if x then x else 0)
         | Nil -> (if x then x elif y then y else 0)
         | Cons x xs -> (if x then x else 0)
         | Bin i r d -> (if x then x else 0)

-- Case/let

def t10 = case (let x1 = y1 in z1)
           | Nil -> (let x2 = y2 in z2)
           | Cons x xs -> (let x3 = y3 in z3)
           | Bin i r d -> (let x4 = y4 in z4)

-- Case/lambda

def t11 = case (\\x1->x1)
          | Nil -> (\\x2->x2)
          | Cons x xs -> (\\x3->x3)
          | Bin i r d -> (\\x4->x4)

-- Case/secuencia

def t12 = case (a ; b)
          | Nil -> (c ; d)
          | Cons x xs -> (e ; f ; g)
          | Bin i r d -> (h ; i ; j)

-- Let/if

def t13 = let x = (if a1 then b1 else c1) in
            if a2 then b2 else c2

-- Let/elif

def t14 = let x = (if a1 then b1 elif c1 then d1 elif e1 then f1 else g1) in
            if a2 then b2
          elif c2 then d2
          elif e2 then f2
                  else g2

-- Let/case

def t15 = let x = (case a1
                  | Nil -> b1
                  | Cons x xs -> c1
                  | Bin i r d -> d1)
            in case a2
                  | Nil -> b2
                  | Cons x xs -> c2
                  | Bin i r d -> d2

-- Let/lambda

def t16 = let x = (\\ x1 y1 -> z1)
            in \\ x2 y2 -> z2

-- Let/secuencia

def t17 = let x = (
a;b;c
)in(d;e;f
)

-- Lambda/if

def t18 = \\ x y z -> if a then b else c

-- Lambda/elif

def t19 = \\ x -> if a then b elif c then d else e

-- Lambda/case

def t20 = \\ x y -> case y

-- Lambda/let

def t21 = \\ x -> let a = b in c

-- Lambda/secuencia

def t22 = \\ x -> (a ; b ; c)

'''

data13 = '''
-- Operadores

def t1 = a || b
def t2 = a && b
def t3 = ! a
def t4 = a == b
def t5 = a != b
def t6 = a >= b
def t7 = a <= b
def t8 = a > b
def t9 = a < b
def t10 = a + b
def t11 = a - b
def t12 = a * b
def t13 = a / b
def t14 = a % b
def t15 = - a

'''

data14 = '''
-- Asociatividad

def t1=a||b||c||d
def t2=a&&b&&c&&d
def t3=!!!!!a
def t4=a+b+c+d
def t5=a-b-c-d
def t6=a+b-c+d-e+f
def t7=a*b*c*d
def t8=a/b/c/d
def t9=a%b%c%d
def t10=a/b%c/d%e/f
def t9=- - - -a
def it1=a||(b||(c||d))
def it2=a&&(b&&(c&&d))
def it4=a+(b+(c+d))
def it5=a-(b-(c-d))
def it6=a+(b-(c+(d-(e+f))))
def it7=a*(b*(c*d))
def it8=a/(b/(c/d))
def it9=a%(b%(c%d))
def it10=a/(b%(c/(d%(e/f))))
'''

data15 = '''
-- Anidamiento operadores/aplicación

def t1 = f a b || g c d
def t2 = f a b && g c d
def t3 = ! f a b
def t4 = f a b == g c d
def t5 = f a b != g c d
def t6 = f a b >= g c d
def t7 = f a b <= g c d
def t8 = f a b > g c d
def t9 = f a b < g c d
def t10 = f a b + g c d
def t11 = f a b - g c d
def t12 = f a b * g c d
def t13 = f a b / g c d
def t14 = f a b % g c d
def t15 = - f a b

'''

data16 = '''
-- Precedencia

def t1 = -a a % -0 / -A % -'a' * -b b % -1 / -B % -'b' -
         -c c % -2 / -C % -'c' * -d d % -3 / -D % -'d' +
         -e e % -4 / -E % -'e' * -f f % -5 / -F % -'f' -
         -g g % -6 / -G % -'g' * -h h % -7 / -H % -'h'
def t2 = -a%0/'a'*'A'-A a1 a2+a A1 A2==-b%0/'b'*'B'-B b1 b2+b B1 B2
def t3 = -a%0/'a'*'A'-A a1 a2+a A1 A2!=-b%0/'b'*'B'-B b1 b2+b B1 B2
def t4 = -a%0/'a'*'A'-A a1 a2+a A1 A2>=-b%0/'b'*'B'-B b1 b2+b B1 B2
def t5 = -a%0/'a'*'A'-A a1 a2+a A1 A2<=-b%0/'b'*'B'-B b1 b2+b B1 B2
def t6 = -a%0/'a'*'A'-A a1 a2+a A1 A2>-b%0/'b'*'B'-B b1 b2+b B1 B2
def t7 = -a%0/'a'*'A'-A a1 a2+a A1 A2<-b%0/'b'*'B'-B b1 b2+b B1 B2
def t8 = !-a%0/'a'*'A'-A a1 a2+a A1 A2==-b%0/'b'*'B'-B b1 b2+b B1 B2&&
         !-c%0/'c'*'C'-C c1 c2+c C1 C2!=-d%0/'d'*'D'-D d1 d2+d D1 D2||
         !-e%0/'e'*'E'-E e1 e2+e E1 E2>=-f%0/'f'*'F'-F f1 f2+f F1 F2&&
         !-g%0/'g'*'G'-G g1 g2+g G1 G2<=-h%0/'h'*'H'-H h1 h2+h H1 H2||
         !-i%0/'i'*'I'-I i1 i2+i I1 I2>-j%0/'j'*'J'-J j1 j2+j J1 J2&&
         !-k%0/'k'*'K'-K k1 k2+k K1 K2<-l%0/'l'*'L'-L l1 l2+l L1 L2
def t9=-(a;b)
def t10=-(a && b)
def t11=(a == b)*((a != b)+(a < b))*((a <= b)-(a >= b))*(a>b)
'''

data17 = '''

def null list =
  case list
  | Nil       -> True
  | Cons x xs -> False

def head list =
  case list
  | Cons x xs -> x

def tail list =
  case list
  | Cons x xs -> xs

def take n list =
  if n == 0 || null list
   then Nil
   else Cons (head list) (take (n - 1) (tail list))

def sum list =
  if null list
   then 0
   else head list + tail list

def gen n =
  if n == 0
   then Nil
   else Cons n (gen (n - 1))

def main =
  sum (gen 100)

'''

datas = [data, data01, data02, data03, data04, data05, data06, data07, data08, data09, data10, data11, data12, data13, data14, data15, data16, data17]

flecha = Flecha()
flecha.lexer.input(data17)

while True:
    tok = flecha.lexer.token()
    if not tok:
        break  # No more input
    print (tok)

program = flecha.yacc.parse(data17)
#for data in datas:
#    print("------------------------------- AST from input program ------------------------------- ")
#    program = flecha.yacc.parse(data)
#    print(program)
#    print("--------------------------------------------------------------------------------------")


print("------------------------------- AST from input program ------------------------------- ")
print(program)"""