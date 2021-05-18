{-# LANGUAGE TemplateHaskell #-}
module Lens1
    ( name,
    location,
    
    
    ) where

import Control.Lens hiding (element)
import Control.Lens.TH
import Control.Lens.Combinators
import Control.Lens.Operators

type Degrees = Double
type Latitude = Degrees
type Longitude = Degrees

data Meetup = Meetup { _name :: String, _location :: (Latitude, Longitude) }
makeLenses ''Meetup
