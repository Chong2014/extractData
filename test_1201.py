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

    def setRightList(self, rightList):
        self.rightList = rightList
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

    def annotate(self):
        currentSub = 2
        for node in self.string:
            stack = []
            currentNode = node
            while currentNode.subscript == -1:
                #print("sub == -1" + currentNode.name + "," + str(currentNode.superscript))
                stack.append(currentNode)
                if currentNode.parent != None:
                    #print("going up " + currentNode.name)
                    currentNode = currentNode.parent

            while stack:
                node = stack.pop()
                #print("pop node " + node.name)
                node.superscript = node.parent.subscript
                currentSub = currentSub + 1
                node.subscript = currentSub
        return True

    def closeBrackets(self):
        s2 = []
        for key, value in myDict.items():
            if value == "1": # key is three strings: "a", "c", "f"
                s2.append(key) #s2 = ['a', 'c', 'f']
        for node in self.linear:
            stack2 = []
            if node.name in s2: #three ordered strings: f, a, c
                stack2.append(node.name)
                for node.name in stack2:
                    if node.parent != None:
                        if node.parent in self.rightList:
                            node = node.parent
                            stack2.append(node.name)

                print stack2
                print len(stack2)

    def printTree(self):
        if not self.root:
            print("tree is empty")
            return False
        stack = [self.root]
        while stack:
            node = stack.pop()
            if node.right:
                stack.append(node.right)
            if node.left:
                stack.append(node.left)

            if node.subscript == -1 or node.superscript == -1:
                print("Invalid sub/super for node " + node.name)
                return False

            if node in string:
                if node.subscript - node.superscript > 2:
                    print("[\BLab{" + node.name + "}{" + str(node.superscript) + "}{" + str(node.subscript) + "}]")
                else:
                    print("[\Lab{" + node.name + "}{" + str(node.superscript) + "}{" + str(node.subscript) + "}]")

            else:
                if node.subscript - node.superscript > 2:
                    print("[\BLab{" + node.name + "}{" + str(node.superscript) + "}{" + str(node.subscript) + "}")
                else:
                    print("[\Lab{" + node.name + "}{" + str(node.superscript) + "}{" + str(node.subscript) + "}")


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
myDict = {"f":"1", "e":"0", "a":"1", "d":"0", "c":"1", "b":"0"}
rightList = [XP2, XP4, XP8]


tree.setmyDict(myDict)
tree.setString(string)
tree.setLinear(linear)
tree.setRightList(rightList)
tree.annotate()
tree.closeBrackets()
tree.printTree()
