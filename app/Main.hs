module Main where

import Lib
import Data.Sort
import qualified Data.Map as M

type Var = Int

----------------------------------------------- Exp

data Atom = Var Var
          | AVal Bool
          | AFalse deriving (Eq, Show)

data Clause = Clause [Atom]
            | Atom Atom

data Exp = Exp [Clause]
         | EVal Bool


substitute_clause :: Var -> Bool -> Clause -> Clause
substitute_clause var val (Clause (a : as)) = 
    case substitute_clause var val (Clause as) of 
        Clause rest -> Clause (hd : rest)
    where
        val' = AVal val
        hd   = case a of
                v@(Var i) -> if i == var then val' else v
                _  -> a
substitute_clause var val c = c

clause_no_vars :: Clause -> Bool
clause_no_vars (Clause [])       = True
clause_no_vars (Clause (a : as)) = case a of
    Var _ -> False
    _     -> clause_no_vars (Clause as)
clause_no_vars _ = True


substitute' :: Var -> Bool -> Exp -> Exp
substitute' var val (Exp exp) = Exp $ map (substitute_clause var val) exp
substitute' var val b = b


clauseToBools :: Clause -> [Bool]
clauseToBools (Clause []) = []
clauseToBools (Clause (a : as)) = case a of
    AVal b -> b : clauseToBools as'
    _      -> clauseToBools as'
    where as' = Clause as

orl :: [Bool] -> Bool
orl [] = False
orl (b:bs) = b || orl bs

andl :: [Bool] -> Bool
andl [] = True
andl (b:bs) = b && andl bs


evalClause :: Clause -> Bool
evalClause (Clause [])     = False
evalClause c@(Clause _)    = (orl . clauseToBools) c
evalClause (Atom (AVal b)) = b


exp_no_vars :: [Clause] -> Bool
exp_no_vars = andl . (map clause_no_vars)


evalExp :: Exp -> Exp
evalExp exp@(Exp cs@(c:cs')) = 
    if exp_no_vars cs 
        then EVal $ andl $ (map evalClause) cs
        else exp

evalExp exp = exp

substitute :: Var -> Bool -> Exp -> Exp
substitute var val exp = evalExp $ substitute' var val exp


----------------------------------------------- ROBDD


type NodeId = Int

type DMap = M.Map NodeId (NodeId, Var, NodeId)
type RDMap = M.Map (NodeId, Var, NodeId) NodeId

data ROBDD = Node DMap RDMap NodeId
           | Ref ROBDD
           | RVal Bool
           deriving Show

nextId :: RDMap -> Var
nextId map = case (reverse . sort . M.elems) map of
    []       -> 1
    (id : _) -> id + 1

robdd :: NodeId -> Var -> NodeId -> DMap -> RDMap -> ROBDD
robdd low var high map revmap
    | low == high = Node map revmap low
    | otherwise   = case M.lookup (low, var, high) revmap of
        Just n -> Node map revmap n
        Nothing -> robdd' low var high map revmap

robdd' :: NodeId -> Var -> NodeId -> DMap -> RDMap -> ROBDD
robdd' low var high map revmap = 
    Node (M.insert id (low, var, high) map) (M.insert (low, var, high) id revmap) id
    where id = nextId revmap

build :: Exp -> ROBDD
build exp = build' 1 M.empty M.empty exp


build' :: Var -> DMap -> RDMap -> Exp -> ROBDD
build' i map revmap (EVal False) = Node map revmap 0
build' i map revmap (EVal True)  = Node map revmap 1
build' i map revmap exp = 
    let 
        loExp  = substitute i False exp
        hiExp = substitute i True exp
        i' = succ i
        Node loMap loRevmap low = build' i' map revmap loExp
        Node hiMap hiRevmap high = build' i' loMap loRevmap hiExp
    in 
        robdd low i high hiMap hiRevmap



-- (x1 ∧ x2)∨(x1 ∧ x3)∨(x2 ∧ x3)
bexp = Exp [Clause [Var 1, Var 2], Clause [Var 1, Var 3], Clause [Var 2, Var 3]]

x = build bexp




main :: IO ()
main = someFunc
