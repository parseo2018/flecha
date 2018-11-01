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
        r'''[\'].*.[\']'''

    	return t

    def t_STRING(self, t):
        r'''\"(.*?)\"'''

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
    	('right', 'UMINUS')
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
    	''' not_empty_program : program definition '''
    	p[0] = p[1].push(p[2])

    # ******************* Definition *******************

    def p_definition(self, p):
    	''' definition : DEF LOWERID params DEFEQ expression '''
        p[0] = Definition([p[2],p[3],p[5]])

    # ******************* Params *******************

    def p_empty_params(self, p):
        ''' empty : '''
        p[0] = Parameters(children=[])

    def p_params(self, p):
        ''' params : empty_params 
                   | not_empty_params
        '''
        p[0] = p[1]

    def p_not_empty_params(self, p):
        ''' not_empty_params : LOWEID params '''
        p[0] = p[2].push(p[1])

    #outer : Diego
    def p_expression(self, p):
        ''' expression : outer_expression
                       | secuence_expression '''
        p[0] = p[1]

    def p_secuence_expression(self, p):
        ''' secuence_expression : secuence_expression SEMICOLON expression '''
        p[0] = SecuenceExpression(p[1], p[3])

    def p_outer_expression(self, p):
        ''' outer_expression : if_expression
                             | case_expression
                             | let_expression
                             | lamba_expression
                             | inner_expression '''
        p[0] = p[1]


    def p_if_expression(self, p):
        ''' if_expression : IF inner_expression THEN inner_expression branch_else '''
        p[0] = IfExpression(p[2], p[4], p[5])

    def p_case_expression(self, p):
        ''' case_expression : CASE inner_expression branch_case '''
        p[0] = CaseExpression(p[2], p[3])

    def p_let_expression(self, p):    
        ''' let_expression : LET LOWERID params DEFEQ inner_expression IN outer_expression '''
        p[0] = LetExpression(p[2], p[3], p[5], p[7])

    def p_lambda_expression(self, p):    
        ''' lambda_expression : LAMBDA params ARROW outer_expression'''
        p[0] = LambdaExpression(p[2], p[4])

    def p_inner_expression(self, p):
        ''' inner_expression : apply_expression
                             | binary_expression
                             | unary_expression '''
        p[0] = p[1]

    def p_apply_expression(self, p):
        ''' apply_expression : apply_atomic_expression 
                             | atomic_expression '''
        p[0] = p[1]

    def p_atomic_expression(self, p):
        ''' atomic_expression : non_paren_atomic
                              | paren_atomic '''
        p[0] = p[1]
    
    def p_non_paren_atomic(self, p):
        ''' non_paren_atomic : non_paren_atomic '''
        p[0] = AtomicExpression(p[1])

    def p_paren_atomic(self, p):
        ''' paren_atomic : LPAREN paren_atomic RPAREN'''
        p[0] = ParenAtomicExpression(p[2])

    def p_apply_atomic_expression(self, p):
        ''' empty_program : apply_expression atomic_expression '''
        p[0] = AppyAtomicExpression(p[1], p[2])

    def p_binary_expression(self, p):
        ''' binary_expression : inner_expression binary_op inner_expression '''
        p[0] = BinaryExpression(p[1], p[2], p[3])

    def p_unary_expression(self, p):
        ''' unary_expression : unary_op inner_expression'''
        p[0] = UnaryExpression(p[1], p[2]) 

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
'''

flecha = Flecha()
flecha.lexer.input(data)

while True:
    tok = flecha.lexer.token()
    if not tok:
        break  # No more input
    print (tok)



			
