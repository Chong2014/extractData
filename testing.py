
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

    def add(self, node, newNodeName, leftOrRight):
        if self.root == None:
            self.root = Node(newNodeName, 1, 2)
            return self.root
        else:
            if leftOrRight == "0":
                node.left = Node(newNodeName)
                node.left.parent = node
                return node.left
            else:
                node.right = Node(newNodeName)
                node.right.parent = node
                return node.right

    def annotate(self):
        currentSub = 2
        for node in string:
            stack = []
            currentNode = node
            while currentNode.subscript == -1:
                stack.append(currentNode)
                if currentNode.parent != None:
                    currentNode = currentNode.parent

            while stack:
                node = stack.pop()
                node.superscript = node.parent.subscript
                currentSub = currentSub + 1
                node.subscript = currentSub

    def printTree(self):
        stackAll = [self.root]
        stackRight = []
        stackRightName = []
        while stackAll:
            node = stackAll.pop()
            if node.right:
                stackAll.append(node.right)
                stackRight.append(node.right)
                stackRightName.append(node.right.name)
            if node.left:
                stackAll.append(node.left)

            if node in string:
                if node.subscript - node.superscript > 2:
                    print ("[\BLab{" + node.name + "}{" + str(node.superscript) + "}{" + str(node.subscript) + "}]")
                else:
                    print("[\Lab{" + node.name + "}{" + str(node.superscript) + "}{" + str(node.subscript) + "}]")
            else:
                if node.subscript - node.superscript > 2:
                    print("[\BLab{" + node.name + "}{" + str(node.superscript) + "}{" + str(node.subscript) + "}")
                else:
                    print("[\Lab{" + node.name + "}{" + str(node.superscript) + "}{" + str(node.subscript) + "}")

        for node in linear:
            stack2 = []
            if node.name in stackRightName:
                stack2.append(node.name)
                for node.name in stack2:
                    if node.parent != None:
                        if node.parent in stackRight:
                            node = node.parent
                            stack2.append(node.name)
                #print stack2
                #print len(stack2)
            for node in string:
                if node.name in stack2:
                    print (node.name + "]"*len(stack2))

tree = Tree()

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

tree.annotate()
tree.printTree()
