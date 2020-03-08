
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
                                     [ HH.div [ bulmaClass [ BC.unsafeClassName "media-content" ] ] 
                                              [ HH.p [ bulmaClass [ BC.unsafeClassName "title"
                                                                  , BC.unsafeClassName "article-title"
                                                                  , BC.unsafeClassName "has-text-centered"
                                                                  ] 
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
renderPosts _ = 
    HH.div [ bulmaClass [ BCOL.column
                        , BC.unsafeClassName "is-12" 
                        , BC.unsafeClassName "is-offset-0"
                        ] 
           ] $ map renderCard [ testPost1, testPost2 ]



posts :: forall m. H.Component HH.HTML (Const Void) Unit Void m
posts =
    H.mkComponent { initialState: const unit
                  , render: renderPosts
                  , eval: H.mkEval $ H.defaultEval
                  }



renderHome :: forall m. Unit -> H.ComponentHTML Void () m
renderHome _ = 
    HH.section [ bulmaClass [ BC.unsafeClassName "section-heading" ] ] 
               [ HH.h2 [ bulmaClass [ BC.unsafeClassName "title" 
                                    , BC.unsafeClassName "is-1"
                                    , BC.unsafeClassName "has-text-white"
                                    , BC.unsafeClassName "has-text-centered"
                                    ] 
                       ]
                       [ HH.text "Welcome to Chanwoo's blog" ]
 
               , HH.h3 [ bulmaClass [ BC.unsafeClassName "subtitle"
                                    , BC.unsafeClassName "is-3"
                                    , BC.unsafeClassName "has-text-white"
                                    , BC.unsafeClassName "has-text-centered"
                                    ] 
                       ]
                       [ HH.text "Blog constructing in progress" ]
               ]
     




profile :: forall m. H.Component HH.HTML (Const Void) Unit Void m
profile = 
    H.mkComponent { initialState: const unit
                  , render: renderProfile
                  , eval: H.mkEval $ H.defaultEval
                  }




renderProfile :: forall m. Unit -> H.ComponentHTML Void () m
renderProfile _ = 
    HH.div_ [ HH.section [ bulmaClass [ BC.unsafeClassName "section-heading" ] ] 
                         [ HH.h1 [ bulmaClass [ BC.unsafeClassName "title" 
                                              , BC.unsafeClassName "is-1"
                                              , BC.unsafeClassName "has-text-white"
                                              , BC.unsafeClassName "has-text-centered"
                                              ] 
                                 ]
                                 [ HH.text "Lee Chanwoo" ]

                         , HH.h2 [ bulmaClass [ BC.unsafeClassName "subtitle"
                                              , BC.unsafeClassName "is-3"
                                              , BC.unsafeClassName "has-text-white"
                                              , BC.unsafeClassName "has-text-centered"
                                              ] 
                                 ]
                                 [ HH.text "Mathmatic Programmer" ]
                         ]

            , HH.section [ HP.id_ "about" 
                         , bulmaClass [ BC.unsafeClassName "section"
                                      , BC.unsafeClassName "section-padding-large"
                                      ]
                         ]
                         [ HH.div [ bulmaClass [ BC.unsafeClassName "section-heading" ] ] 
                                  [ HH.h3 [ bulmaClass [ BC.unsafeClassName "title" 
                                                       , BC.unsafeClassName "is-2"
                                                       ] 
                                          ] 
                                          [ HH.text "About Me" ]

                                  , HH.h4 [ bulmaClass [ BC.unsafeClassName "subtitle" 
                                                       , BC.unsafeClassName "is-5"
                                                       ] 
                                          ]
                                          [ HH.text "Tech All Rounder" ]

                                  , HH.div [ bulmaClass [ BC.unsafeClassName "container" ] ]
                                           [ HH.p_ [ HH.text "I'm the all rounder of development, system design etc." ] 
                                           ]
                                  ]

                         , HH.div [ bulmaClass [ BC.unsafeClassName "columns" 
                                               , BC.unsafeClassName "has-same-height"
                                               , BC.unsafeClassName "is-gapless"
                                               ] 
                                  ]
                                  [ HH.div [ bulmaClass [ BC.unsafeClassName "column" ] ]
                                           [ HH.div [ bulmaClass [ BC.unsafeClassName "card" ] ] 
                                                    [ HH.div [ bulmaClass [ BC.unsafeClassName "card-content" ] ] 
                                                             [ HH.h3 [ bulmaClass [ BC.unsafeClassName "title" 
                                                                                   , BC.unsafeClassName "is-4"
                                                                                   ] 
                                                                     ] 
                                                                     [ HH.text "Profile" ]

                                                             , HH.div [ bulmaClass [ BC.unsafeClassName "content" ] ]
                                                                      [ HE.table [ bulmaClass [ BC.unsafeClassName "table-profile" ] ]
                                                                                 [ HE.tbody_ [ HE.tr_ [ HE.th [ HP.colSpan 1 ] []
                                                                                                      , HE.th [ HP.colSpan 2 ] []
                                                                                                      ] 
                                                                                             , HE.tr_ [ HE.td_ [ HH.text "Address:" ] 
                                                                                                      , HE.td_ [ HH.text "Guru's Lab" ]
                                                                                                      ]
                                                                                             , HE.tr_ [ HE.td_ [ HH.text "Phone:" ]
                                                                                                      , HE.td_ [ HH.text "0123-456789" ]  
                                                                                                      ]
                                                                                             , HE.tr_ [ HE.td_ [ HH.text "Email:" ]
                                                                                                      , HE.td_ [ HH.text "minion@despicable.me" ]
                                                                                                      ]
                                                                                             ] 
                                                                                 ]
                                                                      ] 


                                                             , HH.br_
                                                             , HH.div [ bulmaClass  [ BC.unsafeClassName "buttons" ] ]
                                                                      [ HH.a [ bulmaClass [ BC.unsafeClassName "button" ] ] 
                                                                             [ HH.text "Github" ]
                                                                      , HH.a [ bulmaClass [ BC.unsafeClassName "button" ] ]
                                                                             [ HH.text "LinkedIn" ]
                                                                      , HH.a [ bulmaClass [ BC.unsafeClassName "button" ] ]
                                                                             [ HH.text "Twitter" ]
                                                                      ]

                                                             ] 
                                                    ]
                                           ]
                                  , HH.div [ bulmaClass [ BC.unsafeClassName "column" ] ]
                                           [ HH.div [ bulmaClass [ BC.unsafeClassName "card" ] ] 
                                                    [ HH.div [ bulmaClass [ BC.unsafeClassName "card-image" ] ] 
                                                             [ HE.figure [ bulmaClass [ BC.unsafeClassName "image" 
                                                                                      , BC.unsafeClassName "is-4by3"
                                                                                      ] 
                                                                         ]
                                                                         [ HE.img [ HP.src "https://source.unsplash.com/random/1280x960"
                                                                                  , HP.alt "Placeholder image"
                                                                                  ] 
                                                                         ]
                                                             ]
                                                    ]
                                           ]


                                  , HH.div [ bulmaClass [ BC.unsafeClassName "column" ] ]
                                           [ HH.div [ bulmaClass [ BC.unsafeClassName "card" ] ]
                                                    [ HH.div [ bulmaClass [ BC.unsafeClassName "card-content" 
                                                                          , BC.unsafeClassName "skills-content"
                                                                          ] 
                                                             ] 
                                                             [ HH.h3 [ bulmaClass [ BC.unsafeClassName "title"
                                                                                  , BC.unsafeClassName "is-4" 
                                                                                  ] 
                                                                     ] 
                                                                     [ HH.text "Skills" ]

                                                             , HH.div [ bulmaClass [ BC.unsafeClassName "content"] ]
                                                                      [ HH.article [ bulmaClass [ BC.unsafeClassName "media" ] ] 
                                                                                   [ HH.div [ bulmaClass [ BC.unsafeClassName "media-content" ] ]
                                                                                            [ HH.div [ bulmaClass [ BC.unsafeClassName "content" ] ] 
                                                                                                     [ HH.p_ [ HE.strong_ [ HH.text "JavaScript" ]
                                                                                                             , HH.br_
                                                                                                             , HE.progress [ bulmaClass [ BC.unsafeClassName "progress"
                                                                                                                                , BC.unsafeClassName "is-primary" 
                                                                                                                                ] 
                                                                                                                   , HP.attr (HH.AttrName "value") "90"
                                                                                                                   , HP.attr (HH.AttrName "max") "100"
                                                                                                                   ]  [] -- progress
                                                                                                             ] -- p_
                                                                                                     ] -- media-content
                                                                                            ] -- content
                                                                                   ] -- media

                                                                      , HH.article [ bulmaClass [ BC.unsafeClassName "media" ] ] 
                                                                                   [ HH.div [ bulmaClass [ BC.unsafeClassName "media-content" ] ]
                                                                                            [ HH.div [ bulmaClass [ BC.unsafeClassName "content" ] ] 
                                                                                                     [ HH.p_ [ HE.strong_ [ HH.text "JavaScript" ]
                                                                                                             , HH.br_
                                                                                                             , HE.progress [ bulmaClass [ BC.unsafeClassName "progress"
                                                                                                                                , BC.unsafeClassName "is-primary" 
                                                                                                                                ] 
                                                                                                                   , HP.attr (HH.AttrName "value") "90"
                                                                                                                   , HP.attr (HH.AttrName "max") "100"
                                                                                                                   ]  [] -- progress
                                                                                                             ] -- p_
                                                                                                     ] -- media-content
                                                                                            ] -- content
                                                                                   ] -- media

                                                                      , HH.article [ bulmaClass [ BC.unsafeClassName "media" ] ] 
                                                                                   [ HH.div [ bulmaClass [ BC.unsafeClassName "media-content" ] ]
                                                                                            [ HH.div [ bulmaClass [ BC.unsafeClassName "content" ] ] 
                                                                                                     [ HH.p_ [ HE.strong_ [ HH.text "JavaScript" ]
                                                                                                             , HH.br_
                                                                                                             , HE.progress [ bulmaClass [ BC.unsafeClassName "progress"
                                                                                                                                , BC.unsafeClassName "is-primary" 
                                                                                                                                ] 
                                                                                                                   , HP.attr (HH.AttrName "value") "90"
                                                                                                                   , HP.attr (HH.AttrName "max") "100"
                                                                                                                   ]  [] -- progress
                                                                                                             ] -- p_
                                                                                                     ] -- media-content
                                                                                            ] -- content
                                                                                   ] -- media

                                                                      , HH.article [ bulmaClass [ BC.unsafeClassName "media" ] ] 
                                                                                   [ HH.div [ bulmaClass [ BC.unsafeClassName "media-content" ] ]
                                                                                            [ HH.div [ bulmaClass [ BC.unsafeClassName "content" ] ] 
                                                                                                     [ HH.p_ [ HE.strong_ [ HH.text "JavaScript" ]
                                                                                                             , HH.br_
                                                                                                             , HE.progress [ bulmaClass [ BC.unsafeClassName "progress"
                                                                                                                                , BC.unsafeClassName "is-primary" 
                                                                                                                                ] 
                                                                                                                   , HP.attr (HH.AttrName "value") "90"
                                                                                                                   , HP.attr (HH.AttrName "max") "100"
                                                                                                                   ]  [] -- progress
                                                                                                             ] -- p_
                                                                                                     ] -- media-content
                                                                                            ] -- content
                                                                                   ] -- media

                                                                      , HH.article [ bulmaClass [ BC.unsafeClassName "media" ] ] 
                                                                                   [ HH.div [ bulmaClass [ BC.unsafeClassName "media-content" ] ]
                                                                                            [ HH.div [ bulmaClass [ BC.unsafeClassName "content" ] ] 
                                                                                                     [ HH.p_ [ HE.strong_ [ HH.text "JavaScript" ]
                                                                                                             , HH.br_
                                                                                                             , HE.progress [ bulmaClass [ BC.unsafeClassName "progress"
                                                                                                                                , BC.unsafeClassName "is-primary" 
                                                                                                                                ] 
                                                                                                                   , HP.attr (HH.AttrName "value") "90"
                                                                                                                   , HP.attr (HH.AttrName "max") "100"
                                                                                                                   ]  [] -- progress
                                                                                                             ] -- p_
                                                                                                     ] -- media-content
                                                                                            ] -- content
                                                                                   ] -- media
                                                                      ] -- content
                                                             ] -- card-content
                                                    ] -- card
                                           ] -- column
                                  ] -- columns
                         ] -- section
            ] -- section-heading
                                        


