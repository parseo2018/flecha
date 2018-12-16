import sys, os
sys.path.insert(0,"../..")

from .node import Node

class Program(Node):
	
	def __init__(self,children=[],leaf=None):
		Node.__init__(self, '', children, leaf)

	def push(self,child):
		self.children = self.children + [child]
		return self