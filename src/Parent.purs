

module Parent where

import Prelude

import Children as Children
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Elements as HE
import Halogen.HTML.Core (ClassName(..))
import Router as R


import Bulma.Common as BC
import Bulma.Layout.Layout as BL
import Bulma.Components.Navbar as BN
import Bulma.Elements.Title as BT




toHClassName :: BC.ClassName -> ClassName 
toHClassName (BC.ClassName str) = ClassName str



bulmaClass :: forall r i. Array BC.ClassName -> HP.IProp (class :: String | r) i 
bulmaClass = HP.class_ <<< ClassName  <<< BC.runClassNames


type ChildSlots = ( home :: Children.Slot Unit
                  , posts :: Children.Slot Unit
                  , profile :: Children.Slot Unit
                  )


_home :: SProxy "home"
_home = SProxy 

_posts :: SProxy "posts"
_posts = SProxy 


_profile :: SProxy "profile"
_profile = SProxy 


type State = { currentRoute :: R.Route  }

data Query a = ChangeRoute R.Route a

type Input = R.Route



page :: forall m. H.Component HH.HTML Query Input Void m
page = H.mkComponent { initialState
                     , render
                     , eval : H.mkEval $ H.defaultEval 
                        { handleQuery = handleQuery }
                     }


initialState :: Input -> State
initialState route = { currentRoute : route }

handleQuery :: forall m a. Query a -> H.HalogenM State Void ChildSlots Void m (Maybe a)
handleQuery (ChangeRoute route k) = do
    H.modify_ _{ currentRoute = route }
    pure $ Just k



renderMenu :: forall m. H.ComponentHTML Void ChildSlots m
renderMenu = HH.nav [ HP.class_ $ toHClassName BN.navbar] 
                    [ HH.div [ HP.class_ $ toHClassName BL.container ] 
                             [ HH.div [ bulmaClass [ BC.unsafeClassName "navbar-brand"] ]
                                      [ HE.span [ bulmaClass [ BC.unsafeClassName "navbar-burger" 
                                                             , BC.unsafeClassName "burger" 
                                                             --, BC.unsafeClassName "is-active"
                                                             ] 
                                                , HP.attr (HH.AttrName "data-target") "navbarMenu"
                                                ]
                                                [ HE.span_ []
                                                , HE.span_ []
                                                , HE.span_ []
                                                ]
                                      ]
                             , HH.div [ HP.id_ "navbarMenu"
                                      , bulmaClass [ BN.navbarMenu
                                                   --, BC.unsafeClassName "is-active" 
                                                   ]
                                      ]

                                      [ HH.div [ HP.class_ $ toHClassName BN.navbarEnd ]
                                               [ HH.a [ bulmaClass [ BN.navbarItem
                                                                   --, BC.unsafeClassName "is-active"
                                                                   ]
                                                      , HP.href "#home" 
                                                      ] 
                                                      [ HH.text "Home"  ]
                                               , HH.a [ bulmaClass [ BN.navbarItem ]
                                                      , HP.href "#posts" 
                                                      ]
                                                      [ HH.text "Posts"  ]
                                               , HH.a [ bulmaClass [ BN.navbarItem ] 
                                                      , HP.href "#profile" 
                                                      ]
                                                      [ HH.text "Profile" ] 

                                               ]
                                      ]
                             ]
                    ]



renderSection :: forall m. H.ComponentHTML Void ChildSlots m
renderSection = HH.section [ bulmaClass [ BL.hero, BL.isBold
                                           , BC.unsafeClassName "is-info"
                                           , BC.unsafeClassName "is-medium" 
                                           ]
                           ]
                           [ HH.div [ HP.class_ $ toHClassName BL.heroBody ]
                                    [ HH.div [ bulmaClass [ BL.container
                                                          , BC.unsafeClassName "has-text-centered"
                                                          ]
                                             ]
                                             [ HH.h1 [ HP.class_ $ toHClassName BT.title ] 
                                                     [ HH.text "Beautiful Programming"
                                                     ]

                                             ]
                                    ]
                           ]


renderPane :: forall m. State -> H.ComponentHTML Void ChildSlots m
renderPane state = HH.div [ bulmaClass [ BL.container ] ]
                          [ HH.section [ bulmaClass [ BC.unsafeClassName "articles" ] ]
                                       [  case state.currentRoute of 
                                              R.Home -> HH.slot _home unit Children.home unit absurd
                                              R.Posts -> HH.slot _posts unit Children.posts unit absurd
                                              R.Profile-> HH.slot _profile unit Children.profile unit absurd
                                       ]
 
                          ]



render :: forall m. State -> H.ComponentHTML Void ChildSlots m
render state = 
    HH.div_ [ renderMenu
            , renderSection 
            , renderPane state
            ]
