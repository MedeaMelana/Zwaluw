-- Chris Done's example at http://hackage.haskell.org/package/url-generic-0.1
-- translated to Zwaluw

{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoMonoPatBinds #-}

import Web.Zwaluw
import Web.Zwaluw.TH

import Prelude hiding (id, (.), (/))
import Control.Category

import Data.Maybe


-- Datatypes copied verbatim from example

data Event = Event { eventId     :: Maybe Integer -- ^ The event id.
                   , eventScope  :: Bool          -- ^ Show the scope?
                   , eventLayout :: Layout        -- ^ Layout for the page.
                   }
  deriving Show

data Layout =
  Wide | Thin | Collapsed
  deriving (Show, Enum)


-- Let Zwaluw automatically derive pure routers

event :: Router (Maybe Integer :- Bool :- Layout :- r) (Event :- r)
event = $(deriveRouterTuple ''Event)

wide, thin, collapsed :: Router r (Layout :- r)
(wide, thin, collapsed) = $(deriveRouterTuple ''Layout)


-- Custom routers, tying a URL format to the datatypes

rEvent :: Router r (Event :- r)
rEvent = event / "event" / "id" / rJust . integer / rFalse . "layout" / rLayout

rLayout :: Router r (Layout :- r)
rLayout = wide      . "wide"
      <> thin      . "thin"
      <> collapsed . "collapsed"


-- Auxiliary functions

parseURLPath :: String -> Maybe Event
parseURLPath = parse1 rEvent

formatURLPath :: Event -> Maybe String
formatURLPath = unparse1 rEvent
