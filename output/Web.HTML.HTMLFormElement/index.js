// Generated by purs version 0.13.6
"use strict";
var $foreign = require("./foreign.js");
var Unsafe_Coerce = require("../Unsafe.Coerce/index.js");
var Web_Internal_FFI = require("../Web.Internal.FFI/index.js");
var toParentNode = Unsafe_Coerce.unsafeCoerce;
var toNonDocumentTypeChildNode = Unsafe_Coerce.unsafeCoerce;
var toNode = Unsafe_Coerce.unsafeCoerce;
var toHTMLElement = Unsafe_Coerce.unsafeCoerce;
var toEventTarget = Unsafe_Coerce.unsafeCoerce;
var toElement = Unsafe_Coerce.unsafeCoerce;
var toChildNode = Unsafe_Coerce.unsafeCoerce;
var fromParentNode = Web_Internal_FFI.unsafeReadProtoTagged("HTMLFormElement");
var fromNonDocumentTypeChildNode = Web_Internal_FFI.unsafeReadProtoTagged("HTMLFormElement");
var fromNode = Web_Internal_FFI.unsafeReadProtoTagged("HTMLFormElement");
var fromHTMLElement = Web_Internal_FFI.unsafeReadProtoTagged("HTMLFormElement");
var fromEventTarget = Web_Internal_FFI.unsafeReadProtoTagged("HTMLFormElement");
var fromElement = Web_Internal_FFI.unsafeReadProtoTagged("HTMLFormElement");
var fromChildNode = Web_Internal_FFI.unsafeReadProtoTagged("HTMLFormElement");
module.exports = {
    fromHTMLElement: fromHTMLElement,
    fromElement: fromElement,
    fromNode: fromNode,
    fromChildNode: fromChildNode,
    fromNonDocumentTypeChildNode: fromNonDocumentTypeChildNode,
    fromParentNode: fromParentNode,
    fromEventTarget: fromEventTarget,
    toHTMLElement: toHTMLElement,
    toElement: toElement,
    toNode: toNode,
    toChildNode: toChildNode,
    toNonDocumentTypeChildNode: toNonDocumentTypeChildNode,
    toParentNode: toParentNode,
    toEventTarget: toEventTarget,
    acceptCharset: $foreign.acceptCharset,
    setAcceptCharset: $foreign.setAcceptCharset,
    action: $foreign.action,
    setAction: $foreign.setAction,
    autocomplete: $foreign.autocomplete,
    setAutocomplete: $foreign.setAutocomplete,
    enctype: $foreign.enctype,
    setEnctype: $foreign.setEnctype,
    encoding: $foreign.encoding,
    setEncoding: $foreign.setEncoding,
    method: $foreign.method,
    setMethod: $foreign.setMethod,
    name: $foreign.name,
    setName: $foreign.setName,
    noValidate: $foreign.noValidate,
    setNoValidate: $foreign.setNoValidate,
    target: $foreign.target,
    setTarget: $foreign.setTarget,
    length: $foreign.length,
    submit: $foreign.submit,
    reset: $foreign.reset,
    checkValidity: $foreign.checkValidity,
    reportValidity: $foreign.reportValidity
};
