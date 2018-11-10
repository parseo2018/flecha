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
        ''' empty_params : '''#empty '''
        #p[0] = Parameters(children=[])

    def p_params(self, p):
        ''' params : '''#empty_params 
                   #| not_empty_params
        #'''
        #p[0] = p[1]

    def p_not_empty_params(self, p):
        ''' not_empty_params : '''#LOWERID params '''
        #p[0] = Parameters() p[2].push(p[1])

    def p_expression(self, p):
        ''' expression : outer_expression'''
                       #| secuence_expression '''           
        p[0] = p[1]

    #def p_secuence_expression(self, p):
    #    ''' secuence_expression : secuence_expression SEMICOLON expression '''
    #    p[0] = SecuenceExpression(p[1], p[3])

    def p_outer_expression(self, p):
        ''' outer_expression : inner_expression'''
                             #| case_expression
                             #| let_expression
                             #| lamba_expression
                             #|  if_expression'''
        p[0] = p[1]


    def p_if_expression(self, p):
        ''' if_expression : '''#IF inner_expression THEN inner_expression branch_else '''
        #p[0] = IfExpression(p[2], p[4], p[5])

    def p_case_expression(self, p):
        ''' case_expression : '''#CASE inner_expression branch_case '''
        #p[0] = CaseExpression(p[2], p[3])

    def p_let_expression(self, p):    
        ''' let_expression : '''#LET LOWERID params DEFEQ inner_expression IN outer_expression '''
        #p[0] = LetExpression(p[2], p[3], p[5], p[7])

    def p_lambda_expression(self, p):    
        ''' lambda_expression : LAMBDA params ARROW outer_expression                  
        '''
        p[0] = LambdaExpression(p[2], p[4])

    def p_inner_expression(self, p):
        ''' inner_expression : apply_expression'''
                             #| binary_expression
                             #| unary_expression '''
        p[0] = p[1]

    def p_apply_expression(self, p):
        ''' apply_expression : atomic_expression '''
                             #| apply_atomic_expression '''
        p[0] = p[1]

    def p_atomic_expression(self, p):
        ''' atomic_expression : non_paren_atomic'''
                              #| paren_atomic '''
        p[0] = p[1]

    def p_non_paren_atomic(self, p):
        ''' non_paren_atomic : char_expression
                             | number_expression
                             | lower_id_expression'''
        p[0] = p[1]


    def p_char_expression(self, p):
        ''' char_expression : CHAR
        '''
        p[1] = str(ord(p[1])) if p[1] != '' else p[1]

        p[0] = ExpressionAtomic("ExprChar", p[1])


    def p_number_expression(self, p):
        ''' number_expression : NUMBER
        '''    
        p[0] = ExpressionAtomic("ExprNumber", str(p[1]))

    def p_lower_id_expression(self, p):
        ''' lower_id_expression : LOWERID
        '''    
        p[0] = ExpressionAtomic("ExprVar", p[1])

    #def p_paren_atomic(self, p):
    #    ''' paren_atomic : LPAREN expression RPAREN'''
    #    p[0] = ParenAtomicExpression(p[2])

    #def p_apply_atomic_expression(self, p):
        #''' empty_program : '''#apply_expression atomic_expression '''
        #p[0] = AppyAtomicExpression(p[1], p[2])

    #def p_binary_expression(self, p):
        #''' binary_expression : '''#inner_expression binary_op inner_expression '''
        #p[0] = BinaryExpression(p[1], p[2], p[3])

    #def p_unary_expression(self, p):
        #''' unary_expression : '''#unary_op inner_expression'''
        #p[0] = UnaryExpression(p[1], p[2]) 

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
	-- Variables                     def A = 1
    --    if  True then x3 = "a" else x3 = "b"
    --if  True then x3 = "a" else x3 = "b"
    --if True then False else True
    --2 * 3 + 3
    --IF IF IF

    --def a b c = a + b + c
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
def abcdefghijklmnopqrstuvwxyz01234567899876543210zyxwvutsrqponmlkjABC_DE_baabcdefghijklmnopqrstuvwxyz01234567899876543210zyxwvutsrqponmlkjihgfedcbaabcdefghijklmnopqrstuvwxyz01234567899876543210zyxwvutsrqponmlkjihgfedcbaabcdefghiFGH_I_J_K_L_Mwxyz01234567899876543210zyxwvutsrqponmlkjihgfedcbaabcdefghijklmnopqrstuvwxyz01234567899876543210zyxwvutsrqponmlkjihgfedcbaabcdefghijklmnopqrstuvwxyz01234567899876543210zyxwvutsrqponmlkjihgfedcbaabcdefghijklmnopqrstuvwxyz01234567899876543210zyxwvutsrqponmlkjihgfedcbaabcdefghijklmnopqrstuvwxyz01234567899876543210zyxwvutsrqponmlkjihgfedcbaabcdefghijklmnopqrstuvwxyz01234567899876543210zyxwvutsrqponmlkjihgfedcbaabcdefghijklmnopqrstuvwxyz01234567899876543210zyxwvutsrqponmlkjihgfedcbaabcdefghijklmnopqrstuvwxyz01234567899876543210zyxwvutsrqponmlkjihgfedcbaabcdefghijklmnopqrstuvwxyz01234567899876543210zyxwvutsrqponmlkjihgfedcbaabcdefghijklmnopqrstuvwxyz01234567899876543210zyxwvutsrqponmlkjihgfedcbaabcdefghijklmnopqrstuvwxyz01234567899876543210zyxwvutsrqponmlkj = y
'''

data02 = \
    '''
    def a = 'a' def z = 'z' def a_ = 'A' def z_ = 'z'
def cero = '0' def nueve = '9' def espacio = ' ' def tab = '\t'
def cr = '\r' def lf = '\n' def comilla = '\'' def doble_comilla = '\"'
def contrabarra = '\\' def igual = '=' def lparen = '(' def rparen = ')'
-- Caracteres
'''

flecha = Flecha()
flecha.lexer.input(data02)

while True:
    tok = flecha.lexer.token()
    if not tok:
        break  # No more input
    print (tok)

program = flecha.yacc.parse(data02)

print("------------------------------- AST from input program ------------------------------- ")
print(program)
			
