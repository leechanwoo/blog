
module Children where

import Prelude 

import Data.Const (Const)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Elements as HE
import Halogen.HTML.Core (ClassName(..))

import Bulma.Common as BC
import Bulma.Layout.Layout as BL
import Bulma.Columns.Columns as BCOL

type Slot = H.Slot (Const Void) Void


bulmaClass :: forall r i. Array BC.ClassName -> HP.IProp (class :: String | r) i 
bulmaClass = HP.class_ <<< ClassName  <<< BC.runClassNames



home :: forall m. H.Component HH.HTML (Const Void) Unit Void m
home =
    H.mkComponent { initialState: const unit
                  , render: renderHome
                  , eval: H.mkEval $ H.defaultEval
                  }


--renderHome :: forall m. Unit -> H.ComponentHTML Void () m
--renderHome _ = HH.ul_ [ HH.li_ [ HH.text "Adrianne" ] 
--                       , HH.li_ [ HH.text "Carolus" ]
--                       ]


renderHome :: forall m. Unit -> H.ComponentHTML Void () m
renderHome _ = HH.div [ bulmaClass [ BCOL.column
                                   , BC.unsafeClassName "is-8" 
                                   , BC.unsafeClassName "is-offset-2"
                                   ] 
                      ]
                      [ HH.div [ bulmaClass [ BC.unsafeClassName "card", BC.unsafeClassName "article"] ]
                               [ HH.div [ bulmaClass [ BC.unsafeClassName "card-content" ] ]
                                        [ HH.div [ bulmaClass [ BC.unsafeClassName "media" ] ] 
                                                 [ HH.div [ bulmaClass [ BC.unsafeClassName "media-content" 
                                                                       , BC.unsafeClassName "has-text-centered"
                                                                       ] 
                                                          ] 
                                                          [ HH.p [ bulmaClass [ BC.unsafeClassName "title"
                                                                              , BC.unsafeClassName "article-title"] 
                                                                 ]
                                                                 [ HH.text "Test Post1" ]

                                                          , HH.div [ bulmaClass [ BC.unsafeClassName "content"
                                                                                , BC.unsafeClassName "article-body"
                                                                                ] 
                                                                   ]
                                                                   [ HH.p_ [ HH.text "This is test contents" 
                                                                           , HE.br_
                                                                           , HH.text "This is test contents"
                                                                           , HE.br_
                                                                           , HH.text "This is test contents"
                                                                           , HE.br_
                                                                           , HH.text "This is test contents"
                                                                           , HE.br_
                                                                           ]
                                                                   ]

                                                          ]
                                                 ]
                                        ]
                               ]
                      , HH.div [ bulmaClass [ BC.unsafeClassName "card"
                                            , BC.unsafeClassName "article"
                                            ] 
                               ]
                               [ HH.div [ bulmaClass [ BC.unsafeClassName "card-content" ] ]
                                        [ HH.div [ bulmaClass [ BC.unsafeClassName "media" ] ] 
                                                 [ HH.div [ bulmaClass [ BC.unsafeClassName "media-content" 
                                                                       , BC.unsafeClassName "has-text-centered"
                                                                       ] 
                                                          ] 
                                                          [ HH.p [ bulmaClass [ BC.unsafeClassName "title"
                                                                              , BC.unsafeClassName "article-title"] 
                                                                 ]
                                                                 [ HH.text "Test Post1" ]

                                                          , HH.div [ bulmaClass [ BC.unsafeClassName "content"
                                                                                , BC.unsafeClassName "article-body"
                                                                                ] 
                                                                   ]
                                                                   [ HH.p_ [ HH.text "This is test contents" 
                                                                           , HE.br_
                                                                           , HH.text "This is test contents"
                                                                           , HE.br_
                                                                           , HH.text "This is test contents"
                                                                           , HE.br_
                                                                           , HH.text "This is test contents"
                                                                           , HE.br_
                                                                           ]
                                                                   ]

                                                          ]
                                                 ]
                                        ]
                               ]
                      , HH.div [ bulmaClass [ BC.unsafeClassName "card", BC.unsafeClassName "article"] ]
                               [ HH.div [ bulmaClass [ BC.unsafeClassName "card-content" ] ]
                                        [ HH.div [ bulmaClass [ BC.unsafeClassName "media" ] ] 
                                                 [ HH.div [ bulmaClass [ BC.unsafeClassName "media-content" 
                                                                       , BC.unsafeClassName "has-text-centered"
                                                                       ] 
                                                          ] 
                                                          [ HH.p [ bulmaClass [ BC.unsafeClassName "title"
                                                                              , BC.unsafeClassName "article-title"] 
                                                                 ]
                                                                 [ HH.text "Test Post1" ]

                                                          , HH.div [ bulmaClass [ BC.unsafeClassName "content"
                                                                                , BC.unsafeClassName "article-body"
                                                                                ] 
                                                                   ]
                                                                   [ HH.p_ [ HH.text "This is test contents" 
                                                                           , HE.br_
                                                                           , HH.text "This is test contents"
                                                                           , HE.br_
                                                                           , HH.text "This is test contents"
                                                                           , HE.br_
                                                                           , HH.text "This is test contents"
                                                                           , HE.br_
                                                                           ]
                                                                   ]

                                                          ]
                                                 ]
                                        ]
                               ]
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
