import sys, os
sys.path.insert(0,"../..")

class Node:

	def __init__(self,typeNode,children=None,leaf=None):
	  	self.typeNode = typeNode
	  	if children:
	  		self.children = children
	  	else:
	  		self.children = [ ]
	  		self.leaf = leaf

	def __str__(self, level=0):
		ret = " "*level+ "[\n" +str(self.typeNode)
		ret = ret + ", " + self.leaf if self.children == [] else ret
		for child in self.children:
			ret += child.__str__(level+1)
		ret += " "*level+ "] \n"
		return ret

class Program(Node):
	
	def __init__(self,children=[],leaf=None):
		Node.__init__(self, '', children, leaf)

	def push(self,child):
		self.children = [child] + self.children
		return self

class Definition(Node):
	
	def __init__(self, children=[], leaf=None):
		Node.__init__(self, 'Def', children, leaf)

	def push(self,child):
		self.children = [child] + self.children
		return self

	def __str__(self, level=0):
		ret = " "*level+ "[" +str(self.typeNode) + ", \"" + self.children[0] + "\","
		ret += self.children[1].__str__(level+1)
		ret += " "*level+ "] \n"
		return ret

class ExpressionAtomic(Node):

  def __init__(self, name, leaf):
    Node.__init__(self, name, [], leaf)	  
