#!/usr/bin/python3
# -*- coding: utf-8 -*-

import sys
import os
sys.path.insert(0, '../..')

from parser import Parser
from ast_representation import *

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
        r'''(\'(.*?)\')'''

        t.value = t.value[1:-1]
        return t

    def t_STRING(self, t):
        r'''\"(.*?)\"'''

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
    	#('right', 'UMINUS')
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
        p[0] = Definition(children=p[2] + [LambdaExpression(children=[p[3],p[5]])])

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

    def p_expression(self, p):
        ''' expression : outer_expression
                       | secuence_expression '''           
        p[0] = p[1]

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
        return str(ord(char)) if char != '' else char

    def p_char_expression(self, p):
        ''' char_expression : CHAR
        '''
        p[1] = self.get_char_ord(p[1])

        p[0] = ExpressionAtomic("ExprChar", p[1])

    def p_string_expression(self, p):
        ''' string_expression : STRING
        '''
        if p[1]:
            next_char = p[1][0]
            sub = AppyAtomicExpression(children=[ExpressionAtomic("ExprConstructor", "Cons"), ExpressionAtomic("ExprChar", self.get_char_ord(next_char))])
            p[1] = p[1][1:]
            p[0] = AppyAtomicExpression(children=[sub,self.p_string_expression(p)])
        else:
            p[0] = ExpressionAtomic("ExprConstructor", "Nil")
        return p[0]

    def p_number_expression(self, p):
        ''' number_expression : NUMBER
        '''    
        p[0] = ExpressionAtomic("ExprNumber", str(p[1]))

    def p_lower_id_expression(self, p):
        ''' lower_id_expression : LOWERID
        '''    
        p[0] = ExpressionAtomic("ExprVar", p[1])

    def p_upper_id_expression(self, p):
        ''' upper_id_expression : UPPERID
        '''    
        p[0] = ExpressionAtomic("ExprConstructor", p[1])

    def p_paren_atomic(self, p):
        ''' paren_atomic : LPAREN expression RPAREN'''
        p[0] = p[2]

    def p_apply_atomic_expression(self, p):
        ''' apply_atomic_expression : apply_expression atomic_expression '''
        p[0] = AppyAtomicExpression(children=[p[1], p[2]])

    def p_binary_expression(self, p):
        ''' binary_expression : inner_expression binary_op inner_expression '''
        subExpr = AppyAtomicExpression(children=[p[1], p[2]])
        p[0] = AppyAtomicExpression(children=[subExpr, p[3]])

    def p_binary_op(self, p):
        '''binary_op : AND
                     | OR
                     | EQ
                     | NE
                     | GE
                     | LE
                     | GT
                     | LT
                     | PLUS
                     | MINUS
                     | TIMES
                     | DIV
                     | MOD
                     '''
        p[0] = ExpressionAtomic("ExprVar", p[1])

    def p_unary_expression(self, p):
        ''' unary_expression : unary_op inner_expression'''
        p[0] = AppyAtomicExpression(children=[p[2], p[1]])

    def p_unary_op(self, p):
        '''unary_op : NOT
                    | MINUS'''
        p[0] = ExpressionAtomic("ExprVar", p[1])

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

data02 = \
    '''
    def a = 'a' def z = 'z' def a_ = 'A' def z_ = 'z'
def cero = '0' def nueve = '9' def espacio = ' ' def tab = '\t'
def cr = '\r' def lf = '\n' 
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

datas = [data, data01, data02, data03, data04, data05, data06, data07, data08, data09, data10, data11]

flecha = Flecha()
flecha.lexer.input(data11)

while True:
    tok = flecha.lexer.token()
    if not tok:
        break  # No more input
    print (tok)

program = flecha.yacc.parse(data11)
#for data in datas:
#    print("------------------------------- AST from input program ------------------------------- ")
#    program = flecha.yacc.parse(data)
#    print(program)
#    print("--------------------------------------------------------------------------------------")


print("------------------------------- AST from input program ------------------------------- ")
print(program)