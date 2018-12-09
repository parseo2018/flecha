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
		ret += " "*level+ "]\n"
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
		ret = " "*level+ "[\"" +str(self.typeNode) + "\", \"" + self.children[0] + "\", \n"
		ret += self.children[1].__str__(level+1)
		ret += "\n" + " "*level+ "]\n"
		return ret

class ExpressionAtomic(Node):
	
	def __init__(self, name, leaf):
		Node.__init__(self, name, [], leaf)

	def __str__(self, level=0):
		ret = " "*level+ "[\"" +str(self.typeNode) + "\", "
		ret = ret + (self.leaf if self.leaf.isnumeric() else "\"" + self.leaf + "\"") + "]"
		return ret

class AppyAtomicExpression(Node):

	def __init__(self, children=[], leaf=None):
		Node.__init__(self, 'ExprApply', children, leaf)

	def __str__(self, level=0):
		ret = " "*level+ "[\"" +str(self.typeNode) + ",\"\n"
		ret += self.children[0].__str__(level+1) + ", \n"
		ret += self.children[1].__str__(level+1)
		ret += "\n" + " "*level+ "]"
		return ret

class CaseBranch(Node):

	def __init__(self, children=[], leaf=None):
		Node.__init__(self, 'CaseBranch', children, leaf)

	def __str__(self, level=0):
		ret = " "*level+ "[\"" +str(self.typeNode) + "\" , "
		ret += "\"" + self.children[0] + "\", ["
		if self.children[1]:
			ret += self.children[1][0]
			for child in self.children[1][1:]:
				ret += "," + child
		ret += "],\n"
		ret += self.children[2].__str__(level+1) + "\n"
		ret += " "*level+ "]"
		return ret

class CaseExpression(Node):

	def __init__(self, children=[], leaf=None):
		Node.__init__(self, 'ExprCase', children, leaf)

	def __str__(self, level=0):
		ret = " "*level+ "[\"" +str(self.typeNode) + "\"\n"
		ret += self.children[0].__str__(level+1) + ",\n"
		ret += " "*(level+1)+ "["
		if self.children[1]:
			ret += "\n" + self.children[1][0].__str__(level+2)
			for child in self.children[1][1:]:
				ret += ",\n" + child.__str__(level+2)
			ret += "\n" + " "*(level+1)+ "]" + "\n"
		else:
			ret += "]\n"
		ret += " "*level+ "]"
		return ret

class Parameters(Node):

	def __init__(self, children=[]):
		Node.__init__(self, '', children, None)

	def __str__(self, level=0):
		ret = " "*level+ "[\""
		for p in self.children:
			ret += p	
		ret += "]"
		return ret

class LambdaExpression(Node):
	def __init__(self, children=[], leaf=None):
		Node.__init__(self, 'ExprLambda', children, leaf)

	def __str__(self, level=0):
		ret = " "*level+ "[\"" +str(self.typeNode)+", \""+self.children[0]+"\",\n"
		ret += self.children[1].__str__(level+1) + "\n"
		ret += " "*level+ "]"
		return ret