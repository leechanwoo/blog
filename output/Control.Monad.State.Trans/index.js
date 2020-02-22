// Generated by purs version 0.13.6
"use strict";
var Control_Alt = require("../Control.Alt/index.js");
var Control_Alternative = require("../Control.Alternative/index.js");
var Control_Applicative = require("../Control.Applicative/index.js");
var Control_Apply = require("../Control.Apply/index.js");
var Control_Bind = require("../Control.Bind/index.js");
var Control_Lazy = require("../Control.Lazy/index.js");
var Control_Monad = require("../Control.Monad/index.js");
var Control_Monad_Cont_Class = require("../Control.Monad.Cont.Class/index.js");
var Control_Monad_Error_Class = require("../Control.Monad.Error.Class/index.js");
var Control_Monad_Reader_Class = require("../Control.Monad.Reader.Class/index.js");
var Control_Monad_Rec_Class = require("../Control.Monad.Rec.Class/index.js");
var Control_Monad_State_Class = require("../Control.Monad.State.Class/index.js");
var Control_Monad_Trans_Class = require("../Control.Monad.Trans.Class/index.js");
var Control_Monad_Writer_Class = require("../Control.Monad.Writer.Class/index.js");
var Control_MonadPlus = require("../Control.MonadPlus/index.js");
var Control_MonadZero = require("../Control.MonadZero/index.js");
var Control_Plus = require("../Control.Plus/index.js");
var Data_Functor = require("../Data.Functor/index.js");
var Data_Newtype = require("../Data.Newtype/index.js");
var Data_Tuple = require("../Data.Tuple/index.js");
var Data_Unit = require("../Data.Unit/index.js");
var Effect_Class = require("../Effect.Class/index.js");
var StateT = function (x) {
    return x;
};
var withStateT = function (f) {
    return function (v) {
        return function ($105) {
            return v(f($105));
        };
    };
};
var runStateT = function (v) {
    return v;
};
var newtypeStateT = new Data_Newtype.Newtype(function (n) {
    return n;
}, StateT);
var monadTransStateT = new Control_Monad_Trans_Class.MonadTrans(function (dictMonad) {
    return function (m) {
        return function (s) {
            return Control_Bind.bind(dictMonad.Bind1())(m)(function (x) {
                return Control_Applicative.pure(dictMonad.Applicative0())(new Data_Tuple.Tuple(x, s));
            });
        };
    };
});
var mapStateT = function (f) {
    return function (v) {
        return function ($106) {
            return f(v($106));
        };
    };
};
var lazyStateT = new Control_Lazy.Lazy(function (f) {
    return function (s) {
        var v = f(Data_Unit.unit);
        return v(s);
    };
});
var functorStateT = function (dictFunctor) {
    return new Data_Functor.Functor(function (f) {
        return function (v) {
            return function (s) {
                return Data_Functor.map(dictFunctor)(function (v1) {
                    return new Data_Tuple.Tuple(f(v1.value0), v1.value1);
                })(v(s));
            };
        };
    });
};
var execStateT = function (dictFunctor) {
    return function (v) {
        return function (s) {
            return Data_Functor.map(dictFunctor)(Data_Tuple.snd)(v(s));
        };
    };
};
var evalStateT = function (dictFunctor) {
    return function (v) {
        return function (s) {
            return Data_Functor.map(dictFunctor)(Data_Tuple.fst)(v(s));
        };
    };
};
var monadStateT = function (dictMonad) {
    return new Control_Monad.Monad(function () {
        return applicativeStateT(dictMonad);
    }, function () {
        return bindStateT(dictMonad);
    });
};
var bindStateT = function (dictMonad) {
    return new Control_Bind.Bind(function () {
        return applyStateT(dictMonad);
    }, function (v) {
        return function (f) {
            return function (s) {
                return Control_Bind.bind(dictMonad.Bind1())(v(s))(function (v1) {
                    var v3 = f(v1.value0);
                    return v3(v1.value1);
                });
            };
        };
    });
};
var applyStateT = function (dictMonad) {
    return new Control_Apply.Apply(function () {
        return functorStateT(((dictMonad.Bind1()).Apply0()).Functor0());
    }, Control_Monad.ap(monadStateT(dictMonad)));
};
var applicativeStateT = function (dictMonad) {
    return new Control_Applicative.Applicative(function () {
        return applyStateT(dictMonad);
    }, function (a) {
        return function (s) {
            return Control_Applicative.pure(dictMonad.Applicative0())(new Data_Tuple.Tuple(a, s));
        };
    });
};
var monadAskStateT = function (dictMonadAsk) {
    return new Control_Monad_Reader_Class.MonadAsk(function () {
        return monadStateT(dictMonadAsk.Monad0());
    }, Control_Monad_Trans_Class.lift(monadTransStateT)(dictMonadAsk.Monad0())(Control_Monad_Reader_Class.ask(dictMonadAsk)));
};
var monadReaderStateT = function (dictMonadReader) {
    return new Control_Monad_Reader_Class.MonadReader(function () {
        return monadAskStateT(dictMonadReader.MonadAsk0());
    }, (function () {
        var $107 = Control_Monad_Reader_Class.local(dictMonadReader);
        return function ($108) {
            return mapStateT($107($108));
        };
    })());
};
var monadContStateT = function (dictMonadCont) {
    return new Control_Monad_Cont_Class.MonadCont(function () {
        return monadStateT(dictMonadCont.Monad0());
    }, function (f) {
        return function (s) {
            return Control_Monad_Cont_Class.callCC(dictMonadCont)(function (c) {
                var v = f(function (a) {
                    return function (s$prime) {
                        return c(new Data_Tuple.Tuple(a, s$prime));
                    };
                });
                return v(s);
            });
        };
    });
};
var monadEffectState = function (dictMonadEffect) {
    return new Effect_Class.MonadEffect(function () {
        return monadStateT(dictMonadEffect.Monad0());
    }, (function () {
        var $109 = Control_Monad_Trans_Class.lift(monadTransStateT)(dictMonadEffect.Monad0());
        var $110 = Effect_Class.liftEffect(dictMonadEffect);
        return function ($111) {
            return $109($110($111));
        };
    })());
};
var monadRecStateT = function (dictMonadRec) {
    return new Control_Monad_Rec_Class.MonadRec(function () {
        return monadStateT(dictMonadRec.Monad0());
    }, function (f) {
        return function (a) {
            var f$prime = function (v) {
                var v1 = f(v.value0);
                return Control_Bind.bind((dictMonadRec.Monad0()).Bind1())(v1(v.value1))(function (v2) {
                    return Control_Applicative.pure((dictMonadRec.Monad0()).Applicative0())((function () {
                        if (v2.value0 instanceof Control_Monad_Rec_Class.Loop) {
                            return new Control_Monad_Rec_Class.Loop(new Data_Tuple.Tuple(v2.value0.value0, v2.value1));
                        };
                        if (v2.value0 instanceof Control_Monad_Rec_Class.Done) {
                            return new Control_Monad_Rec_Class.Done(new Data_Tuple.Tuple(v2.value0.value0, v2.value1));
                        };
                        throw new Error("Failed pattern match at Control.Monad.State.Trans (line 87, column 16 - line 89, column 40): " + [ v2.value0.constructor.name ]);
                    })());
                });
            };
            return function (s) {
                return Control_Monad_Rec_Class.tailRecM(dictMonadRec)(f$prime)(new Data_Tuple.Tuple(a, s));
            };
        };
    });
};
var monadStateStateT = function (dictMonad) {
    return new Control_Monad_State_Class.MonadState(function () {
        return monadStateT(dictMonad);
    }, function (f) {
        return StateT((function () {
            var $112 = Control_Applicative.pure(dictMonad.Applicative0());
            return function ($113) {
                return $112(f($113));
            };
        })());
    });
};
var monadTellStateT = function (dictMonadTell) {
    return new Control_Monad_Writer_Class.MonadTell(function () {
        return monadStateT(dictMonadTell.Monad0());
    }, (function () {
        var $114 = Control_Monad_Trans_Class.lift(monadTransStateT)(dictMonadTell.Monad0());
        var $115 = Control_Monad_Writer_Class.tell(dictMonadTell);
        return function ($116) {
            return $114($115($116));
        };
    })());
};
var monadWriterStateT = function (dictMonadWriter) {
    return new Control_Monad_Writer_Class.MonadWriter(function () {
        return monadTellStateT(dictMonadWriter.MonadTell0());
    }, function (m) {
        return function (s) {
            return Control_Bind.bind(((dictMonadWriter.MonadTell0()).Monad0()).Bind1())(Control_Monad_Writer_Class.listen(dictMonadWriter)(m(s)))(function (v) {
                return Control_Applicative.pure(((dictMonadWriter.MonadTell0()).Monad0()).Applicative0())(new Data_Tuple.Tuple(new Data_Tuple.Tuple(v.value0.value0, v.value1), v.value0.value1));
            });
        };
    }, function (m) {
        return function (s) {
            return Control_Monad_Writer_Class.pass(dictMonadWriter)(Control_Bind.bind(((dictMonadWriter.MonadTell0()).Monad0()).Bind1())(m(s))(function (v) {
                return Control_Applicative.pure(((dictMonadWriter.MonadTell0()).Monad0()).Applicative0())(new Data_Tuple.Tuple(new Data_Tuple.Tuple(v.value0.value0, v.value1), v.value0.value1));
            }));
        };
    });
};
var monadThrowStateT = function (dictMonadThrow) {
    return new Control_Monad_Error_Class.MonadThrow(function () {
        return monadStateT(dictMonadThrow.Monad0());
    }, function (e) {
        return Control_Monad_Trans_Class.lift(monadTransStateT)(dictMonadThrow.Monad0())(Control_Monad_Error_Class.throwError(dictMonadThrow)(e));
    });
};
var monadErrorStateT = function (dictMonadError) {
    return new Control_Monad_Error_Class.MonadError(function () {
        return monadThrowStateT(dictMonadError.MonadThrow0());
    }, function (v) {
        return function (h) {
            return function (s) {
                return Control_Monad_Error_Class.catchError(dictMonadError)(v(s))(function (e) {
                    var v1 = h(e);
                    return v1(s);
                });
            };
        };
    });
};
var altStateT = function (dictMonad) {
    return function (dictAlt) {
        return new Control_Alt.Alt(function () {
            return functorStateT(dictAlt.Functor0());
        }, function (v) {
            return function (v1) {
                return function (s) {
                    return Control_Alt.alt(dictAlt)(v(s))(v1(s));
                };
            };
        });
    };
};
var plusStateT = function (dictMonad) {
    return function (dictPlus) {
        return new Control_Plus.Plus(function () {
            return altStateT(dictMonad)(dictPlus.Alt0());
        }, function (v) {
            return Control_Plus.empty(dictPlus);
        });
    };
};
var alternativeStateT = function (dictMonad) {
    return function (dictAlternative) {
        return new Control_Alternative.Alternative(function () {
            return applicativeStateT(dictMonad);
        }, function () {
            return plusStateT(dictMonad)(dictAlternative.Plus1());
        });
    };
};
var monadZeroStateT = function (dictMonadZero) {
    return new Control_MonadZero.MonadZero(function () {
        return alternativeStateT(dictMonadZero.Monad0())(dictMonadZero.Alternative1());
    }, function () {
        return monadStateT(dictMonadZero.Monad0());
    });
};
var monadPlusStateT = function (dictMonadPlus) {
    return new Control_MonadPlus.MonadPlus(function () {
        return monadZeroStateT(dictMonadPlus.MonadZero0());
    });
};
module.exports = {
    StateT: StateT,
    runStateT: runStateT,
    evalStateT: evalStateT,
    execStateT: execStateT,
    mapStateT: mapStateT,
    withStateT: withStateT,
    newtypeStateT: newtypeStateT,
    functorStateT: functorStateT,
    applyStateT: applyStateT,
    applicativeStateT: applicativeStateT,
    altStateT: altStateT,
    plusStateT: plusStateT,
    alternativeStateT: alternativeStateT,
    bindStateT: bindStateT,
    monadStateT: monadStateT,
    monadRecStateT: monadRecStateT,
    monadZeroStateT: monadZeroStateT,
    monadPlusStateT: monadPlusStateT,
    monadTransStateT: monadTransStateT,
    lazyStateT: lazyStateT,
    monadEffectState: monadEffectState,
    monadContStateT: monadContStateT,
    monadThrowStateT: monadThrowStateT,
    monadErrorStateT: monadErrorStateT,
    monadAskStateT: monadAskStateT,
    monadReaderStateT: monadReaderStateT,
    monadStateStateT: monadStateStateT,
    monadTellStateT: monadTellStateT,
    monadWriterStateT: monadWriterStateT
};
