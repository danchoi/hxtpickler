{-# LANGUAGE OverloadedStrings #-}  
module Main where
import Text.XML.HXT.Core
import Data.Map (Map)

import Data.Map
 
data Season = Season
 { sYear    :: Int
 , sLeagues :: Leagues
 } deriving (Show, Eq)
 
type Leagues = Map String Divisions
 
type Divisions = Map String [Team]
 
data Team = Team
 { teamName :: String
 , city     :: String
 , players  :: [Player]
 } deriving (Show, Eq)
 
data Player = Player
 { firstName :: String
 , lastName  :: String
 , position  :: String
 , atBats    :: Maybe Int
 , hits      :: Maybe Int
 } deriving (Show, Eq)


instance XmlPickler Season where
   xpickle = xpSeason
 
instance XmlPickler Team where
   xpickle = xpTeam
 
instance XmlPickler Player where
   xpickle = xpPlayer


xpSeason        :: PU Season
xpSeason
   = xpElem "SEASON" $
     xpWrap ( uncurry Season
            , \ s -> (sYear s, sLeagues s)) $
     xpPair (xpAttr "YEAR" xpickle) xpLeagues

xpLeagues       :: PU Leagues
xpLeagues
   = xpWrap ( fromList
            , toList ) $
     xpList $
     xpElem "LEAGUE" $
     xpPair (xpAttr "NAME" xpText) xpDivisions

xpDivisions     :: PU Divisions
xpDivisions
   = xpWrap ( fromList
            , toList
            ) $
     xpList $
     xpElem "DIVISION" $
     xpPair (xpAttr "NAME" xpText) xpickle

xpTeam  :: PU Team
xpTeam
   = xpElem "TEAM" $
     xpWrap ( uncurry3 Team
            , \ t -> ( teamName t
                     , city t
                     , players t
                     )
            ) $
     xpTriple (xpAttr "NAME" xpText)
              (xpAttr "CITY" xpText)
              (xpList xpickle)

xpPlayer        :: PU Player
xpPlayer
   = xpElem "PLAYER" $
     xpWrap ( \ ((f,l,p,a,h)) -> Player f l p a h 
            , \ t -> (firstName t, lastName t
                     , position t, atBats t
                     , hits t
                     )
            ) $
     xp5Tuple (xpAttr           "GIVEN_NAME" xpText  )
              (xpAttr           "SURNAME"    xpText  )
              (xpAttr           "POSITION"   xpText  )
              (xpOption (xpAttr "AT_BATS"    xpickle))
              (xpOption (xpAttr "HITS"       xpickle))

 
main    :: IO ()
main
   = do
     runX ( xunpickleDocument xpSeason
                              [ withValidate no
                              , withTrace 1
                              , withRemoveWS yes
                              , withPreserveComment no
                              ] "season.xml"
            >>>
            processSeason
            >>>
            xpickleDocument   xpSeason
                              [ withIndent yes
                              ] "new-simple2.xml"
          )
     return ()
 
-- the dummy for processing the unpickled data
 
processSeason   :: IOSArrow Season Season
processSeason
   = arrIO ( \ x -> do {print x ; return x})

