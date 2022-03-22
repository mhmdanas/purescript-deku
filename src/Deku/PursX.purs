module Deku.PursX where

import Prim.Row (class Union)
import Prim.RowList (class RowToList, Cons, Nil, RowList)
import Prim.Symbol as Sym

data E (tag :: Symbol) (attr :: Row Symbol) (children :: Row Type)
data T (text :: Symbol)

class IsValidTagOrAttr (h :: Symbol) (t :: Symbol)

instance isValidTagOrAttrA0 :: IsValidTagOrAttr "a" ""
else instance isValidTagOrAttrB0 :: IsValidTagOrAttr "b" ""
else instance isValidTagOrAttrC0 :: IsValidTagOrAttr "c" ""
else instance isValidTagOrAttrD0 :: IsValidTagOrAttr "d" ""
else instance isValidTagOrAttrE0 :: IsValidTagOrAttr "e" ""
else instance isValidTagOrAttrF0 :: IsValidTagOrAttr "f" ""
else instance isValidTagOrAttrG0 :: IsValidTagOrAttr "g" ""
else instance isValidTagOrAttrH0 :: IsValidTagOrAttr "h" ""
else instance isValidTagOrAttrI0 :: IsValidTagOrAttr "i" ""
else instance isValidTagOrAttrJ0 :: IsValidTagOrAttr "j" ""
else instance isValidTagOrAttrK0 :: IsValidTagOrAttr "k" ""
else instance isValidTagOrAttrL0 :: IsValidTagOrAttr "l" ""
else instance isValidTagOrAttrM0 :: IsValidTagOrAttr "m" ""
else instance isValidTagOrAttrN0 :: IsValidTagOrAttr "n" ""
else instance isValidTagOrAttrO0 :: IsValidTagOrAttr "o" ""
else instance isValidTagOrAttrP0 :: IsValidTagOrAttr "p" ""
else instance isValidTagOrAttrQ0 :: IsValidTagOrAttr "q" ""
else instance isValidTagOrAttrR0 :: IsValidTagOrAttr "r" ""
else instance isValidTagOrAttrS0 :: IsValidTagOrAttr "s" ""
else instance isValidTagOrAttrT0 :: IsValidTagOrAttr "t" ""
else instance isValidTagOrAttrU0 :: IsValidTagOrAttr "u" ""
else instance isValidTagOrAttrV0 :: IsValidTagOrAttr "v" ""
else instance isValidTagOrAttrW0 :: IsValidTagOrAttr "w" ""
else instance isValidTagOrAttrX0 :: IsValidTagOrAttr "x" ""
else instance isValidTagOrAttrY0 :: IsValidTagOrAttr "y" ""
else instance isValidTagOrAttrZ0 :: IsValidTagOrAttr "z" ""
else instance isValidTagOrAttrA1 :: (Sym.Cons a b r, IsValidTagOrAttr a b) => IsValidTagOrAttr "a" r
else instance isValidTagOrAttrB1 :: (Sym.Cons a b r, IsValidTagOrAttr a b) => IsValidTagOrAttr "b" r
else instance isValidTagOrAttrC1 :: (Sym.Cons a b r, IsValidTagOrAttr a b) => IsValidTagOrAttr "c" r
else instance isValidTagOrAttrD1 :: (Sym.Cons a b r, IsValidTagOrAttr a b) => IsValidTagOrAttr "d" r
else instance isValidTagOrAttrE1 :: (Sym.Cons a b r, IsValidTagOrAttr a b) => IsValidTagOrAttr "e" r
else instance isValidTagOrAttrF1 :: (Sym.Cons a b r, IsValidTagOrAttr a b) => IsValidTagOrAttr "f" r
else instance isValidTagOrAttrG1 :: (Sym.Cons a b r, IsValidTagOrAttr a b) => IsValidTagOrAttr "g" r
else instance isValidTagOrAttrH1 :: (Sym.Cons a b r, IsValidTagOrAttr a b) => IsValidTagOrAttr "h" r
else instance isValidTagOrAttrI1 :: (Sym.Cons a b r, IsValidTagOrAttr a b) => IsValidTagOrAttr "i" r
else instance isValidTagOrAttrJ1 :: (Sym.Cons a b r, IsValidTagOrAttr a b) => IsValidTagOrAttr "j" r
else instance isValidTagOrAttrK1 :: (Sym.Cons a b r, IsValidTagOrAttr a b) => IsValidTagOrAttr "k" r
else instance isValidTagOrAttrL1 :: (Sym.Cons a b r, IsValidTagOrAttr a b) => IsValidTagOrAttr "l" r
else instance isValidTagOrAttrM1 :: (Sym.Cons a b r, IsValidTagOrAttr a b) => IsValidTagOrAttr "m" r
else instance isValidTagOrAttrN1 :: (Sym.Cons a b r, IsValidTagOrAttr a b) => IsValidTagOrAttr "n" r
else instance isValidTagOrAttrO1 :: (Sym.Cons a b r, IsValidTagOrAttr a b) => IsValidTagOrAttr "o" r
else instance isValidTagOrAttrP1 :: (Sym.Cons a b r, IsValidTagOrAttr a b) => IsValidTagOrAttr "p" r
else instance isValidTagOrAttrQ1 :: (Sym.Cons a b r, IsValidTagOrAttr a b) => IsValidTagOrAttr "q" r
else instance isValidTagOrAttrR1 :: (Sym.Cons a b r, IsValidTagOrAttr a b) => IsValidTagOrAttr "r" r
else instance isValidTagOrAttrS1 :: (Sym.Cons a b r, IsValidTagOrAttr a b) => IsValidTagOrAttr "s" r
else instance isValidTagOrAttrT1 :: (Sym.Cons a b r, IsValidTagOrAttr a b) => IsValidTagOrAttr "t" r
else instance isValidTagOrAttrU1 :: (Sym.Cons a b r, IsValidTagOrAttr a b) => IsValidTagOrAttr "u" r
else instance isValidTagOrAttrV1 :: (Sym.Cons a b r, IsValidTagOrAttr a b) => IsValidTagOrAttr "v" r
else instance isValidTagOrAttrW1 :: (Sym.Cons a b r, IsValidTagOrAttr a b) => IsValidTagOrAttr "w" r
else instance isValidTagOrAttrX1 :: (Sym.Cons a b r, IsValidTagOrAttr a b) => IsValidTagOrAttr "x" r
else instance isValidTagOrAttrY1 :: (Sym.Cons a b r, IsValidTagOrAttr a b) => IsValidTagOrAttr "y" r
else instance isValidTagOrAttrZ1 :: (Sym.Cons a b r, IsValidTagOrAttr a b) => IsValidTagOrAttr "z" r
else instance isValidTagOrAttr_1 :: (Sym.Cons a b r, IsValidTagOrAttr a b) => IsValidTagOrAttr "-" r
class AToXML (t :: RowList Symbol) (s :: Symbol) | t -> s

instance atoXMLNil :: AToXML Nil ""
instance atoXMLCons :: (
  Sym.Cons attrh attrt attr
  , IsValidTagOrAttr attrh attrt
, Sym.Append attr "=" s0
,  Sym.Append s0 "\"" s1
,  Sym.Append s1 val s2
,  Sym.Append s2 "\"" s3
,  Sym.Append s3 " " s4
,  AToXML rest o'
,  Sym.Append s4 o' o
) => AToXML (Cons attr val rest) o

class CToXML (d :: Symbol) (t :: RowList Type) (s :: Symbol) (r :: Row Symbol) | d t -> s r

instance ctoXMLNil :: CToXML d Nil "" ()
instance ctoXMLCons :: (
  ToXML d val s0 r1
,  CToXML d rest o' r2
, Union r1 r2 r3
,  Sym.Append s0 o' o
) => CToXML d (Cons ignore val rest) o r3

class ToXML (d :: Symbol) (t :: Type) (s :: Symbol) (r :: Row Symbol) | d t -> s r
instance toXMLE :: (
  Sym.Cons tagh tagt tag
  , IsValidTagOrAttr tagh tagt
  , Sym.Append "<" tag s0
  , Sym.Append s0 " " s1
  , RowToList attr attrRL
  , AToXML attrRL s2
  , Sym.Append s1 s2 s3
  , Sym.Append s3 ">" s4
  , RowToList children childrenRL
  , CToXML d childrenRL s5 r
  , Sym.Append s4 s5 s6
  , Sym.Append s6 "</" s7
  , Sym.Append s7 tag s8
  , Sym.Append s8 ">" o
) => ToXML d (E tag attr children) o r

instance toXMLT :: ExtractUsingSep d sym r => ToXML d (T sym) sym r

class ExtractUsingSep (d :: Symbol) (sym :: Symbol) (r :: Row Symbol) | d sym -> r

instance ExtractUsingSep d sym r