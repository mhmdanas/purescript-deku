-- | Term-level representations of the typelevel graph. These are
-- | emitted by `run` and can be used to visualize the dom graph.
-- | These representations are also used interally to speed up graph
-- | rendering and to validate the graph in unit tests.
module Deku.Rendered where

import Prelude

import Data.Function (on)
import Data.Map (Map)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Variant (Variant, inj, match)
import Deku.Graph.Attribute (AttributeValue)
import Foreign (Foreign)
import Simple.JSON as JSON
import Type.Proxy (Proxy(..))
import Web.DOM as Web.DOM

newtype ToCreate = ToCreate Foreign
newtype ToChange = ToChange Foreign
instance Eq ToCreate where
  eq = eq `on` show
instance Ord ToCreate where
  compare = compare `on` show
instance Eq ToChange where
  eq = eq `on` show
instance Ord ToChange where
  compare = compare `on` show

instance showToCreate :: Show ToCreate where
  show = JSON.writeJSON
instance showToChange :: Show ToChange where
  show = JSON.writeJSON

instance writeJSONToCreate :: JSON.WriteForeign ToCreate where
  writeImpl (ToCreate tc) = JSON.writeImpl tc
instance readJSONToCreate :: JSON.ReadForeign ToCreate where
  readImpl tc = ToCreate <$> JSON.readImpl tc

instance writeJSONToChange :: JSON.WriteForeign ToChange where
  writeImpl (ToChange tc) = JSON.writeImpl tc
instance readJSONToChange :: JSON.ReadForeign ToChange where
  readImpl tc = ToChange <$> JSON.readImpl tc

newtype RootDOMElement = RootDOMElement Web.DOM.Element
instance Eq RootDOMElement where
  eq _ _ = true
instance Ord RootDOMElement where
  compare _ _ = EQ

instance showRootDOMElement :: Show RootDOMElement where
  show _ = "<root>"

type DisconnectXFromY =
  { fromId :: String
  , fromUnit :: String
  , toId :: String
  , toUnit :: String
  }
type MassiveCreate = { toCreate :: ToCreate }
type MassiveChange = { toChange :: ToChange }
type DestroyUnit = { id :: String, unit :: String }
type MakeElement =
  { id :: String
  , tag :: String
  , attributes :: Array { key :: String, value :: AttributeValue }
  }
type MakeText = { id :: String, text :: String }
type MakeRoot = { id :: String, root :: RootDOMElement }
type MakeSubgraph =
  { id :: String
  , terminus :: String
  , instructions :: Array (Array Instruction)
  }
type MakeTumult =
  { id :: String
  , terminus :: String
  , instructions :: Map Int (Maybe (Array Instruction))
  }
type ConnectXToY =
  { fromId :: String
  , fromUnit :: String
  , toId :: String
  , toUnit :: String
  }
type SetText = { id :: String, text :: String }
type SetAttribute =
  { id :: String
  , key :: String
  , value :: AttributeValue
  }
type SetSubgraph = { id :: String }
type SetTumult =
  { id :: String
  , terminus :: String
  , instructions :: Map Int (Maybe (Array Instruction))
  }

-- An dom rendering instruction. These instructions are used
-- for testing purposes during "dry run" simulations of dom rendering.
-- `Instruction` can also be used if web-dom is being used to control other dom units.

type Instruction' =
  ( disconnectXFromY :: DisconnectXFromY
  , destroyUnit :: DestroyUnit
  , massiveCreate :: MassiveCreate
  , makeElement :: MakeElement
  , makeText :: MakeText
  , makeRoot :: MakeRoot
  , makeSubgraph :: MakeSubgraph
  , makeTumult :: MakeTumult
  , connectXToY :: ConnectXToY
  , massiveChange :: MassiveChange
  , setAttribute :: SetAttribute
  , setText :: SetText
  , setSubgraph :: SetSubgraph
  , setTumult :: SetTumult
  )

newtype Instruction = Instruction (Variant Instruction')

instructionWeight :: Instruction -> Int
instructionWeight (Instruction v) = v # match
  { disconnectXFromY: const 0
  , destroyUnit: const 1
  , makeRoot: const 2
  , makeElement: const 2
  , massiveCreate: const 2
  , makeText: const 2
  , makeSubgraph: const 3
  , makeTumult: const 4
  , connectXToY: const 5
  , setAttribute: const 6
  , massiveChange: const 6
  , setText: const 6
  , setSubgraph: const 7
  , setTumult: const 8
  }

instructionId :: Instruction -> Maybe String
instructionId (Instruction v) = v # match
  { disconnectXFromY: _.fromId >>> Just
  , destroyUnit: _.id >>> Just
  , makeElement: _.id >>> Just
  , makeText: _.id >>> Just
  , makeRoot: _.id >>> Just
  , makeSubgraph: _.id >>> Just
  , massiveCreate: const Nothing
  , makeTumult: _.id >>> Just
  , connectXToY: _.fromId >>> Just
  , setAttribute: _.id >>> Just
  , setText: _.id >>> Just
  , setSubgraph: _.id >>> Just
  , massiveChange: const Nothing
  , setTumult: _.id >>> Just
  }

derive instance newtypeInstruction :: Newtype Instruction _
derive instance eqInstruction :: Eq Instruction
instance ordInstruction :: Ord Instruction where
  compare v1@(Instruction v1') v2@(Instruction v2') = case compare w1 w2 of
    EQ -> c2 unit
    x -> x
    where
    w1 = instructionWeight v1
    w2 = instructionWeight v2
    c2 _ = case compare i1 i2 of
      EQ -> compare v1' v2'
      x -> x
      where
      i1 = instructionId v1
      i2 = instructionId v2

instance showInstruction :: Show Instruction where
  show (Instruction i) = show i

iDisconnectXFromY :: DisconnectXFromY -> Instruction
iDisconnectXFromY = Instruction <<< inj (Proxy :: Proxy "disconnectXFromY")

iDestroyUnit :: DestroyUnit -> Instruction
iDestroyUnit = Instruction <<< inj (Proxy :: Proxy "destroyUnit")

iMakeRoot :: MakeRoot -> Instruction
iMakeRoot = Instruction <<< inj (Proxy :: Proxy "makeRoot")

iMakeText :: MakeText -> Instruction
iMakeText = Instruction <<< inj (Proxy :: Proxy "makeText")

iMakeElement :: MakeElement -> Instruction
iMakeElement = Instruction <<< inj (Proxy :: Proxy "makeElement")

iMakeSubgraph :: MakeSubgraph -> Instruction
iMakeSubgraph = Instruction <<< inj (Proxy :: Proxy "makeSubgraph")

iMassiveCreate :: MassiveCreate -> Instruction
iMassiveCreate = Instruction <<< inj (Proxy :: Proxy "massiveCreate")

iMakeTumult :: MakeTumult -> Instruction
iMakeTumult = Instruction <<< inj (Proxy :: Proxy "makeTumult")

iConnectXToY :: ConnectXToY -> Instruction
iConnectXToY = Instruction <<< inj (Proxy :: Proxy "connectXToY")

iSetAttribute :: SetAttribute -> Instruction
iSetAttribute = Instruction <<< inj (Proxy :: Proxy "setAttribute")

iSetSubgraph :: SetSubgraph -> Instruction
iSetSubgraph = Instruction <<< inj (Proxy :: Proxy "setSubgraph")

iSetTumult :: SetTumult -> Instruction
iSetTumult = Instruction <<< inj (Proxy :: Proxy "setTumult")

iSetText :: SetText -> Instruction
iSetText = Instruction <<< inj (Proxy :: Proxy "setText")

iMassiveChange :: MassiveChange -> Instruction
iMassiveChange = Instruction <<< inj (Proxy :: Proxy "massiveChange")
