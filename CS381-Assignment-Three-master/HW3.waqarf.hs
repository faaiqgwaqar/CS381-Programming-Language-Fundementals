-- Faaiq Gohar Waqar
module HW3 where
import Prelude hiding (Num)
import Data.List

-- Part One
data Mode =  Down
	| Up
	deriving(Eq,Show)

data Expr = R Var
	| L Num
	| Add Expr Expr
	deriving(Eq,Show)

data Cmd = Pen Mode
	| Move Expr Expr
	| Define Macro [Var] Prog
	| Call Macro [Expr]
	deriving(Eq,Show)

type Num = Int
type Var = String
type Macro = String
type Prog = [Cmd]

-- Part Two
-- Concrete Syntax:
-- define line (x1,x2,x3,x4) {
-- 	pen up; move (x1,y1);
-- 	pen down; move (x2,y2);
-- };

line :: Cmd
line = Define "line" ["x1","x2","y1","y2"] [
	Pen Up, Move (R "x1") (R "y1"),
	Pen Down, Move (R "x2") (R "y2")]

-- Part Three
-- Concrete Syntax:
-- define nix (x,y,w,h){
-- 	call line (x,y,x+w,y+h);
-- 	call line (x,y+h,x+w,y);
-- };

nix :: Cmd
nix = Define "nix" ["x","y","w","h"] [
	Call "line" [(R "x"),(R "y"),(Add (R "x") (R "w")),(Add (R "y") (R "h"))],
	Call "line" [(R "x"),(Add (R "y") (R "h")),(Add (R "x") (R "w")),(R "y")]]

-- Part Four
-- Step Function
steps :: Int ->  Prog
steps 0 = [Pen Up, Move (L 0) (L 0), Pen Down]
steps x = steps (x-1) ++ [Move (L (x-1)) (L (x)), Move (L (x)) (L(x))]

-- Part Five
-- Macros Function
macros :: Prog -> [Macro]
macros [] = []
macros ((Pen _):xs) = macros xs
macros ((Move _ _):xs) = macros xs
macros ((Call _ _):xs) = macros xs
macros ((Define m _ _):xs) = [m] ++ macros xs

-- Part Six
-- Pretty Function
modeConverter :: Mode -> String
modeConverter Down = "down"
modeConverter Up = "up"

exprConverter :: Expr -> String
exprConverter (R v) = v
exprConverter (L n) = show n
exprConverter (Add e f) = exprConverter e ++ " + " ++ exprConverter f

exprRecurser :: [Expr] -> [String]
exprRecurser [] = []
exprRecurser (x:xs) = [exprConverter x]  ++ exprRecurser xs

pretty :: Prog -> String
pretty [] = ""
pretty ((Pen d):xs) = "pen " ++ (modeConverter d)  ++ "; " ++ (pretty xs)
pretty ((Move e f):xs) = "move (" ++ (exprConverter e) ++ "," ++ (exprConverter f) ++ "); " ++ (pretty xs)
pretty ((Call m y):xs) = "call " ++ m ++ " (" ++ (intercalate "," (exprRecurser y)) ++ "); " ++ (pretty xs)
pretty ((Define m z w):xs) ="define " ++ m ++ "(" ++ (intercalate "," z) ++ ")" ++ "{" ++ (pretty w) ++ "}; " ++ (pretty xs)
