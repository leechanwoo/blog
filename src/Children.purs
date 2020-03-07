
module Children where

import Prelude 

import Data.Const (Const)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Elements as HE
import Halogen.HTML.Core (ClassName(..), HTML)

import Bulma.Common as BC
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


newtype Content p i = Content 
    { title :: String
    , content :: Array (HTML p i)
    , date :: String
    , tag :: String
    }


testPost1 :: forall p i. Content p i
testPost1  = Content { title: title, date: date, tag: tag, content: content }
    where title = "This is title of test post1"
          date = "March 7, 2020"
          tag = "purescript" 
          content =
              [ HH.p_ [ HH.text "This is a test contents. I'm happy to create my own blog"
                    , HE.br_
                    , HH.text "This is very meaningful to me."
                    , HE.br_
                    , HH.text "Here is for introducing algorithms about machine learning and functional programming. these thems are very important to follow the trand of tech."
                    , HE.br_
                    , HH.text "I'll try to share much useful information "
                    , HE.br_
                    , HH.text "Thank you very much"
                    ]
                  ]


testPost2 :: forall p i. Content p i
testPost2  = Content { title: title, date: date, tag: tag, content: content }
    where title = "This is title of test post1"
          date = "March 7, 2020"
          tag = "functional_programming" 
          content =
              [ HH.p_ [ HH.text "This is test contents" 
                    , HE.br_
                    , HH.text "This is test contents"
                    , HE.br_
                    , HH.text "This is test contents"
                    , HE.br_
                    , HH.text "This is test contents"
                    ]
                  ]






renderCard :: forall p i. Content p i -> HTML p i
renderCard (Content { title: title  
                    , content: content    
                    , date: date
                    , tag: tag
                    }) = 
            HH.div [ bulmaClass [ BC.unsafeClassName "card"
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
                                                     [ HH.text title ]

                                              , HH.div [ bulmaClass [ BC.unsafeClassName "tags"
                                                                    , BC.unsafeClassName "has-addons"
                                                                    , BC.unsafeClassName "level-item" 
                                                                    ] 
                                                       ]
                                                       [ HH.span [ bulmaClass [ BC.unsafeClassName "tag"
                                                                              , BC.unsafeClassName "is-rounded"
                                                                              , BC.unsafeClassName "is-info"
                                                                              ]
                                                                 ] 
                                                                 [ HH.text tag ]

                                                       , HH.span [ bulmaClass [ BC.unsafeClassName "tag"
                                                                              , BC.unsafeClassName "is-rounded"
                                                                              ]
                                                                 ] 
                                                                 [ HH.text date ]

                                                       ]

                                              , HH.div [ bulmaClass [ BC.unsafeClassName "content"
                                                                    , BC.unsafeClassName "article-body"
                                                                    ] 
                                                       ] content
                                              ]
                                     ]
                            ]
                   ]





renderPosts :: forall m. Unit -> H.ComponentHTML Void () m
renderPosts _ = HH.div [ bulmaClass [ BCOL.column
                                   , BC.unsafeClassName "is-8" 
                                   , BC.unsafeClassName "is-offset-2"
                                   ] 
                      ] $ map renderCard [ testPost1, testPost2 ]






posts :: forall m. H.Component HH.HTML (Const Void) Unit Void m
posts =
    H.mkComponent { initialState: const unit
                  , render: renderPosts
                  , eval: H.mkEval $ H.defaultEval
                  }



renderHome :: forall m. Unit -> H.ComponentHTML Void () m
renderHome _ = HH.ul_ [ HH.li_ [ HH.text "Maris Piper" ] 
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
