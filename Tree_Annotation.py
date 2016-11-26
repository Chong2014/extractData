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

    def setList(self, list):
        self.list = list
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

    def annotate(self):
        currentSub = 2
        for node in self.list:
            stack = []
            currentNode = node
            while currentNode.subscript == -1:
                #print("sub == -1" + currentNode.name + "," + str(currentNode.superscript))
                stack.append(currentNode)
                if currentNode.parent != None:
                    #print("going up" + currentNode.name)
                    currentNode = currentNode.parent

            while stack:
                node = stack.pop()
                #print("pop node " + node.name)
                node.superscript = node.parent.subscript
                currentSub = currentSub + 1
                node.subscript = currentSub
        return True

    def printTree(self):
        if not self.root:
            print("tree is empty")
            return False
        stack = [self.root]
        while stack:
            node = stack.pop()
            if node.subscript == -1 or node.superscript == -1:
                print("Invalid sub/super for node " + node.name)
                return False
            if node.subscript - node.superscript > 2:
                if node in list:
                    print("[\BLab{" + node.name + "}{" + str(node.superscript) + "}{" + str(node.subscript) + "}]")
                else:
                    print("[\BLab{" + node.name + "}{" + str(node.superscript) + "}{" + str(node.subscript) + "}")
            else:
                if node in list:
                    print("[\Lab{" + node.name + "}{" + str(node.superscript) + "}{" + str(node.subscript) + "}]")
                else:
                    print("[\Lab{" + node.name + "}{" + str(node.superscript) + "}{" + str(node.subscript) + "}")

            if node.right:
                stack.append(node.right)
            if node.left:
                stack.append(node.left)
tree = Tree()

#TP = tree.add(None, "TP", "")
#Tbar = tree.add(TP, "Tbar", "1")
#T = tree.add(Tbar, "T", "0")
#VP = tree.add(Tbar, "VP", "1")
#John = tree.add(VP, "John", "0")
#Vbar = tree.add(VP, "Vbar", "1")
#likes = tree.add(Vbar, "likes", "0")
#Mary = tree.add(Vbar, "Mary", "1")

#list = [John, T, likes, Mary]

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
list = [a, b, c, d, e, f]

tree.setList(list)
tree.annotate()
tree.printTree()
