import itertools
class Node:
    def __init__(self, name, superscript = -1, subscript = -1):
        self.name = name
        self.left = None
        self.right = None
        self.parent = None
        self.subscript = subscript
        self.superscript = superscript
class Tree:
    def __init__(self):
        self.root = None
        self.num = 1

    def getRoot(self):
        return self.root

    def setString(self, string):
        self.string = string
        return

    def setLinear(self, linear):
        self.linear = linear
        return

    def setmyDict(self, myDict):
        self.myDict = myDict
        return

    def add(self, node, newNodeName, leftOrRight):
        if(self.root == None):
            self.root = Node(newNodeName, 1, 2)
            return self.root
        else:
            if(leftOrRight == "0"):
                node.left = Node(newNodeName)
                node.left.parent = node
                return node.left
            else:
                node.right = Node(newNodeName)
                node.right.parent = node
                return node.right

    def traversePreOrderTree(self):
        if not self.root:
            print("tree is empty")
            return False
        stack = [self.root]
        s1 = []
        while stack:
            node = stack.pop()
            s1.append(str("[") + node.name)
            if node.right:
                stack.append(node.right)
            if node.left:
                stack.append(node.left)
        print s1
            #print ("going up" + node.name)

    def addCloseBrackets(self):
        s2 = []
        for key, value in myDict.items():
            if value == "1": # key is three strings: "a", "c", "f"
                s2.append(key)
        #print s2
        stack2 = []
        for node in self.linear:
            currentNode = node
            while currentNode.subscript == -1:
                for node.name in s2:
                    stack2.append(node.name)
                    if node.parent != None:
                        node = node.parent
                        print("going up " + node.name)










tree = Tree()

#TP = tree.add(None, "TP", "")
#Tbar = tree.add(TP, "Tbar", "1")
#T = tree.add(Tbar, "T", "0")
#VP = tree.add(Tbar, "VP", "1")
#John = tree.add(VP, "John", "0")
#Vbar = tree.add(VP, "Vbar", "1")
#likes = tree.add(Vbar, "likes", "0")
#Mary = tree.add(Vbar, "Mary", "1")

#string = [John, T, likes, Mary]

XP1 = tree.add(None, "XP1", "")
XP2 = tree.add(XP1, "XP2", "1")
XP3 = tree.add(XP2, "XP3", "0")
XP4 = tree.add(XP2, "XP4", "1")
b = tree.add(XP3, "b", "0")
c = tree.add(XP3, "c", "1")
XP7 = tree.add(XP4, "XP7", "0")
XP8 = tree.add(XP4, "XP8", "1")
d = tree.add(XP7, "d", "0")
a = tree.add(XP7, "a", "1")
e = tree.add(XP8, "e", "0")
f = tree.add(XP8, "f", "1")

string = [a, b, c, d, e, f]
linear = [f, e, a, d, c, b]
myDict = {"f":"1", "e":"0", "a":"1", "d":"0", "c":"1", "b":"0"}

tree.setmyDict(myDict)
tree.setString(string)
tree.setLinear(linear)
tree.traversePreOrderTree()
tree.addCloseBrackets()
