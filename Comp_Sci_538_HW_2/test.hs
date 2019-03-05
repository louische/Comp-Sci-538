ext :: Int
ext = 42

myFun = \arg -> arg + ext

data Color = Red | Green | Blue

whatColor :: Color -> String
whatColor Red = "It is red"
whatColor Green = "It is green"
whatColor Blue = "It is blue"


data SameP a = Mkp a a 
