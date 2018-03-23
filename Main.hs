#!/usr/bin/env stack
-- stack runghc

--------------------------------

-- HOW TO RUN:

-- stack runghc Main.hs

--------------------------------


-- Declaration of the module name
module Main
-- List of declarations to export
where

-- import other modules
import DB.Utils

-- Module body - declarations


-- Replace `ex1` with the exercise you want to print

main =
  -- ex1
  -- ex2
  -- ex3
  -- ex4
  -- ex5
  -- ex6
  -- ex8
  -- ex9
  -- ex10
  ex11
  -- ex12
  -- ex13

ex1 = putStrLn "Hello World!"

---------------------------------------------------------------------
-- NOTE: ANSWERS BELOW, DELETE THIS IF YOU ARE JUST GETTING STARTED
---------------------------------------------------------------------






------------------------




















---------------------
-- YOUR WORK BELOW --
---------------------




-- 2 -- define a table structure and an example, use show to print it

type Value = Integer

type Column = (String, Value)

type Row = [Column]

type Table = [Row]

t1 :: Table
t1 =
  [ [ ("x", 1), ("y", 2) ]
  , [ ("x", 3), ("y", 4) ]
  , [ ("x", 4), ("y", 5) ]
  , [ ("x", 6), ("y", 7) ]
  ]

ex2 = print t1

-- 3 -- validate table structure

ppTableData table =
  if checkTable table
     then ppTable table
     else "Inconsistent data"

ex3 = putStrLn (ppTableData t1)

-- 4 -- define database and an example

type Database = [(String, Table)]

db1 =
  [ ("t1", t1)
  , ("t2", t1)
  , ("t3", t3)
  ]

t3 =
  [ [ ("x", 1) ]
  ]

mymap func list =
  if null list
    then []
    else func (head list) : mymap func (tail list)

ppDBNames db =
  intercalate ", " (mymap getName db)

ex4 = putStrLn (ppDBNames db1)

-- 5 -- interact with user input, print the table the user names

prompt = do
  putStrLn ("These are the available tables: " ++ ppDBNames db1 ++ ".")
  putStrLn "which would you like to print?"
  input <- getLine
  let mTable = lookup (trim input) db1
  if isJust mTable
    then
      putStrLn (ppTableData (fromJust mTable))
    else
      putStrLn ("Table named " ++ trim input ++ " not found.")

ex5 = prompt

-- 6 -- Define a Query structure and write an example

ex6 = do
  print (Table "t1")
  print (Values t1)

-- The structure of a query
data Query
  = Values Table
  | Table String

----------------------------------------------------------
-- 8 -- Add a Select structure
  | Select [(String, String)] Query

----------------------------------------------------------
-- 12 -- Add a Union structure
  | Union Query Query

----------------------------------------------------------
-- 13 -- Add a Restrict structure
  | Restrict Cond Query
  deriving (Show, Read)

-- The structure of a condition
data Cond
  = Equals Arg Arg
  | Not Cond
  | And Cond Cond
  deriving (Show, Read)

-- An argument to a condition
data Arg
  = ArgLit Value
  | ArgCol String
  deriving (Show, Read)

-- 8 --------------------------------

sampleSelectQuery =
  Select
    [("x", "x1"), ("x", "x2")]
    (Table "t1")

ex8 = print sampleSelectQuery

-- 12 -------------------------------

sampleUnionedQuery =
  Union
    sampleSelectQuery
    sampleSelectQuery

-- 13 -------------------------------

sampleRestrictedQuery =
  Restrict
    (Equals (ArgLit 1) (ArgCol "x2"))
    sampleSelectQuery

----------------------------------------------------------
-- 9 -- Interpret a simple mathematical expression

data Expr
  = Value Integer
  | Add Expr Expr
  | Mul Expr Expr

eval :: Expr -> Integer
eval expr =
  case expr of
    Value v -> v
    Add e1 e2 -> eval e1 + eval e2
    Mul e1 e2 -> eval e1 * eval e2

myExpr =
  Mul
    (Add (Value 1) (Value 2))
    (Add (Value 2) (Value 5))

ex9 = print (eval myExpr)

----------------------------------------------------------
-- 10 -- Interpret query

interpret :: Database -> Query -> Table
interpret db query = case query of
  Values table ->
    table

  Table tableName ->
    case lookup tableName db of
      Nothing -> error ("Could not find table " ++ tableName ++ " in db.")
      Just t -> t

  Select selectList innerQuery ->
    let
      innerQueryResult =
        interpret db innerQuery

      renameCol row input =
        case input of
          (col, newName) -> (newName, lookupCol col row)

      selectRow row =
        map (renameCol row) selectList
    in
      map selectRow innerQueryResult

-----------------------------------------------------------
-- 12 -- Add Union interpretation

  Union q1 q2 ->
    let
      innerQueryResult1 = interpret db q1
      innerQueryResult2 = interpret db q2
      result = innerQueryResult1 ++ innerQueryResult2
    in
      if checkTable result
        then result
        else error "The two inner queries used by UNION do not have the same schema"

-----------------------------------------------------------
-- 13 -- Add Restrict interpretation

  Restrict cond innerQuery ->
    let
      innerQueryResult = interpret db innerQuery
    in
      filter (interpretCond cond) innerQueryResult

interpretCond cond row = case cond of
  Equals arg1 arg2 ->
    interpretArg row arg1 == interpretArg row arg2

  Not cond ->
    not (interpretCond cond row)

  And cond1 cond2 ->
    interpretCond cond1 row && interpretCond cond2 row

interpretArg row arg = case arg of
  ArgLit v -> v
  ArgCol name -> lookupCol name row

--------------

ex10 = do
  print sampleSelectQuery
  putStrLn (ppTableData (interpret db1 sampleSelectQuery))

ex12 = do
  print sampleUnionedQuery
  putStrLn (ppTableData (interpret db1 sampleUnionedQuery))

ex13 = do
  print sampleRestrictedQuery
  putStrLn (ppTableData (interpret db1 sampleRestrictedQuery))


-----------------------------------------------------------
-- 11 -- a REPL for queries

ex11 :: IO ()
ex11 = repl interpreterPrompt
  where
    interpreterPrompt = do
      input <- getLine
      putStrLn (ppTableData (interpret db1 (readQuery input)))

-----------------------------------------------------------

-- 14 -- Optional -- Add a network server for querying the data

{-

handleSocketMsgs processMsg soc = do
  msg <- recv soc 4096
  result <- catch
    (pure (processMsg msg))
    (\(SomeException e) -> pure (show e))
  _ <- send soc (result ++ "\n")
  pure ()


handleQuery msg = case reads msg of
  [(query,_)] -> ppTableData (interpret db1 query)
  x -> "Could not read query. Got: " ++ show x

runServer = runServerLoop 1337 (handleSocketMsgs handleQuery)

-- connect via `telnet localhost 1337`
-- Send a query, for example, `Table "t1"`

ex14 = runServer

-}
