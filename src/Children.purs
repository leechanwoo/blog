
module Children where

import Prelude 

import Data.Const (Const)
import Halogen as H
import Halogen.HTML as HH

type Slot = H.Slot (Const Void) Void

home :: forall m. H.Component HH.HTML (Const Void) Unit Void m
home =
    H.mkComponent { initialState: const unit
                  , render: renderHome
                  , eval: H.mkEval $ H.defaultEval
                  }


renderHome :: forall m. Unit -> H.ComponentHTML Void () m
renderHome _ = HH.ul_ [ HH.li_ [ HH.text "Adrianne" ] 
                       , HH.li_ [ HH.text "Carolus" ]
                       ]


posts :: forall m. H.Component HH.HTML (Const Void) Unit Void m
posts =
    H.mkComponent { initialState: const unit
                  , render: renderPosts
                  , eval: H.mkEval $ H.defaultEval
                  }



renderPosts :: forall m. Unit -> H.ComponentHTML Void () m
renderPosts _ = HH.ul_ [ HH.li_ [ HH.text "Maris Piper" ] 
                       , HH.li_ [ HH.text "Spanish Agria" ]
                       , HH.li_ [ HH.text "Cara" ]
                       ]


profile :: forall m. H.Component HH.HTML (Const Void) Unit Void m
profile = 
    H.mkComponent { initialState: const unit
                  , render: renderProfile
                  , eval: H.mkEval $ H.defaultEval
                  }


renderProfile :: forall m. Unit -> H.ComponentHTML Void () m
renderProfile _ = HH.ul_ [ HH.li_ [ HH.text "Athlete" ] 
                       , HH.li_ [ HH.text "Pink Fir" ]
                       ]
