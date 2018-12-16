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