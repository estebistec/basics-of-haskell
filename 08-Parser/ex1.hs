data Token = TokLParen | TokRParen | TokEnd
    deriving (Show, Eq)

lookAhead :: [Char] -> Token
lookAhead [] = TokEnd
lookAhead (c:cs)| c == '(' = TokLParen
                | c == ')' = TokRParen
                | otherwise = error $ "Bad input: " ++ (c:cs)

accept :: [Char] -> [Char]
accept [] = error "Nothing to accept"
accept (c:cs) = cs

data Tree = Node Tree Tree | Leaf
    deriving Show

root, expr, par :: [Char] -> (Tree, [Char])

root str = par str
expr str = -- Par Par
    let (tree, str') = par str
    in
        let (tree', str'') = par str'
        in (Node tree tree', str'')
par str = -- '(' Expr ')' | '(' ')'
    case lookAhead str of
    TokLParen -> 
        let str' = accept str
        in case lookAhead str' of
        TokRParen -> (Leaf, accept str') -- ')' <rest>
        TokLParen -> -- Expr ')' <rest>
            let (tree, str'') = expr str'
            in case lookAhead str'' of
            TokLParen -> error "No matching right-paren for expr"
            TokRParen -> (tree, accept str'')
    TokRParen -> error "Unmatched right-paren"

parse str = let (tree, str') = root str
            in
                if null str' 
                then tree 
                else error $ "Unconsumed string " ++ str'

main = print $ parse "(()(()()))"
