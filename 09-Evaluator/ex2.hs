-- Ex 2. Implement function paren that takes an expression tree and turns it into a string with fully parenthesized expression. For instance, when acting on testExpr it should produce the string (x = ((2.0 * (y = 5.0)) + 3.0))

data Operator = Plus | Minus | Times | Div
    deriving (Show, Eq)

data Tree = SumNode Operator Tree Tree
          | ProdNode Operator Tree Tree
          | AssignNode String Tree
          | UnaryNode Operator Tree
          | NumNode Double
          | VarNode String
    deriving Show

paren :: Tree -> String
paren (SumNode op left right) = "(" ++ (paren left) ++ " " ++ (printOp op) ++ " " ++ (paren right) ++ ")"
paren (ProdNode op left right) = "(" ++ (paren left) ++ " " ++ (printOp op) ++ " " ++ (paren right) ++ ")"
paren (AssignNode identifier tree) = "(" ++ identifier ++ " = " ++ (paren tree) ++ ")"
paren (UnaryNode op tree) = (printOp op) ++ (paren tree)
paren (NumNode value) = show value
paren (VarNode identifier) = identifier

printOp :: Operator -> String
printOp Plus = "+"
printOp Minus = "-"
printOp Times = "*"
printOp Div = "/"

-- x = 2 * (y = 5) + 3
testExpr = AssignNode "x" (SumNode Plus 
                             (ProdNode Times 
                               (NumNode 2.0) 
                               (AssignNode "y" (NumNode 5)))
                             (NumNode 3))

main = print $ paren testExpr
