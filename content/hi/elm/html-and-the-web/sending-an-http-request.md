---
date: 2024-01-20 18:00:00.788979-07:00
description: "How to: (\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902:) \u090F\u0932\
  \u094D\u092E \u092E\u0947\u0902 HTTP \u0905\u0928\u0941\u0930\u094B\u0927 \u092D\
  \u0947\u091C\u0928\u0947 \u0915\u0940 \u092A\u094D\u0930\u0915\u094D\u0930\u093F\
  \u092F\u093E \u0938\u093E\u092B \u0914\u0930 \u0928\u093F\u0930\u094D\u092D\u0930\
  \ \u0939\u0948\u0964 \u092F\u0939 \u090F\u0915 \u092A\u094D\u092F\u094B\u0930 \u092B\
  \u0902\u0915\u094D\u0936\u0928\u0932 \u092A\u094D\u0930\u094B\u0917\u094D\u0930\u093E\
  \u092E\u093F\u0902\u0917 \u092D\u093E\u0937\u093E \u0939\u0948, \u091C\u093F\u0938\
  \u0915\u093E \u092E\u0924\u0932\u092C \u0939\u0948 \u0915\u093F \u0938\u093E\u0907\
  \u0921 \u0907\u092B\u0947\u0915\u094D\u091F\u094D\u0938,\u2026"
lastmod: '2024-04-05T22:51:06.868864-06:00'
model: gpt-4-1106-preview
summary: "(\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902:) \u090F\u0932\u094D\u092E\
  \ \u092E\u0947\u0902 HTTP \u0905\u0928\u0941\u0930\u094B\u0927 \u092D\u0947\u091C\
  \u0928\u0947 \u0915\u0940 \u092A\u094D\u0930\u0915\u094D\u0930\u093F\u092F\u093E\
  \ \u0938\u093E\u092B \u0914\u0930 \u0928\u093F\u0930\u094D\u092D\u0930 \u0939\u0948\
  \u0964 \u092F\u0939 \u090F\u0915 \u092A\u094D\u092F\u094B\u0930 \u092B\u0902\u0915\
  \u094D\u0936\u0928\u0932 \u092A\u094D\u0930\u094B\u0917\u094D\u0930\u093E\u092E\u093F\
  \u0902\u0917 \u092D\u093E\u0937\u093E \u0939\u0948, \u091C\u093F\u0938\u0915\u093E\
  \ \u092E\u0924\u0932\u092C \u0939\u0948 \u0915\u093F \u0938\u093E\u0907\u0921 \u0907\
  \u092B\u0947\u0915\u094D\u091F\u094D\u0938, \u091C\u0948\u0938\u0947 HTTP \u0905\
  \u0928\u0941\u0930\u094B\u0927, \u0915\u094B `Cmd` \u0915\u0947 \u092E\u093E\u0927\
  \u094D\u092F\u092E \u0938\u0947 manage \u0915\u093F\u092F\u093E \u091C\u093E\u0924\
  \u093E \u0939\u0948\u0964 \u091C\u093E\u0935\u093E\u0938\u094D\u0915\u094D\u0930\
  \u093F\u092A\u094D\u091F \u092F\u093E \u0905\u0928\u094D\u092F \u092D\u093E\u0937\
  \u093E\u0913\u0902 \u0915\u0940 \u0924\u0930\u0939 Promise \u092F\u093E Callback\
  \ \u0915\u093E \u0907\u0938\u094D\u0924\u0947\u092E\u093E\u0932 \u0928\u0939\u0940\
  \u0902 \u0939\u094B\u0924\u093E\u0964 \u0905\u0924\u0940\u0924 \u092E\u0947\u0902\
  , Ajax \u0914\u0930 XMLHttpRequest \u091C\u0948\u0938\u0947 \u0935\u093F\u092D\u093F\
  \u0928\u094D\u0928 \u0924\u0930\u0940\u0915\u0947 \u0925\u0947 \u091C\u093F\u0938\
  \u0915\u0947 \u0926\u094D\u0935\u093E\u0930\u093E \u0935\u0947\u092C \u092A\u0930\
  \ HTTP \u0905\u0928\u0941\u0930\u094B\u0927 \u0915\u093F\u090F \u091C\u093E\u0924\
  \u0947 \u0925\u0947\u0964 \u090F\u0932\u094D\u092E \u0928\u0947 \u0907\u0938\u0947\
  \ \u0905\u0927\u093F\u0915 \u0938\u0930\u0932 \u0914\u0930 \u0905\u0928\u0941\u092E\
  \u093E\u0928\u093F\u0924 \u092C\u0928\u093E\u092F\u093E \u0939\u0948\u0964 \u0921\
  \u0947\u091F\u093E \u0915\u094B decode \u0915\u0930\u0928\u0947 \u0915\u0947 \u0932\
  \u093F\u090F \u092F\u0939 `Json.Decode` \u0938\u093F\u0938\u094D\u091F\u092E \u0915\
  \u093E \u0909\u092A\u092F\u094B\u0917 \u0915\u0930\u0924\u093E \u0939\u0948, \u091C\
  \u094B \u0915\u093F \u091F\u093E\u0907\u092A \u0938\u0941\u0930\u0915\u094D\u0937\
  \u093F\u0924 \u0939\u0948 \u0914\u0930 \u0930\u0928\u091F\u093E\u0907\u092E \u090F\
  \u0930\u0930 \u0915\u094B \u0915\u092E \u0915\u0930\u0924\u093E \u0939\u0948\u0964\
  \ HTTP \u092A\u0948\u0915\u0947\u091C Elm 0.18 \u092E\u0947\u0902 \u091C\u094B\u0921\
  \u093C\u093E \u0917\u092F\u093E \u0925\u093E, \u0914\u0930 \u0907\u0938\u0947 Elm\
  \ 0.19 \u092E\u0947\u0902 \u0938\u0941\u0927\u093E\u0930\u093E \u0917\u092F\u093E\
  \u0964 \u0907\u0938\u0928\u0947 \u090F\u0932\u094D\u092E \u090F\u092A\u094D\u0932\
  \u093F\u0915\u0947\u0936\u0928\u094D\u0938 \u092E\u0947\u0902 HTTP \u0907\u0902\u091F\
  \u0930\u0947\u0915\u094D\u0936\u0928 \u0915\u094B \u0914\u0930 \u092D\u0940 \u0936\
  \u0915\u094D\u0924\u093F\u0936\u093E\u0932\u0940 \u0914\u0930 \u0932\u091A\u0940\
  \u0932\u093E \u092C\u0928\u093E\u092F\u093E\u0964."
title: "HTTP \u0905\u0928\u0941\u0930\u094B\u0927 \u092D\u0947\u091C\u0928\u093E"
weight: 44
---

## How to: (कैसे करें:)
```Elm
import Http
import Json.Decode exposing (Decoder, string)

type alias User =
    { name : String
    , age : Int
    }

userDecoder : Decoder User
userDecoder =
    Json.Decode.map2 User
        (Json.Decode.field "name" string)
        (Json.Decode.field "age" Json.Decode.int)

fetchUser : Cmd Msg
fetchUser =
    Http.get
        { url = "https://api.example.com/user"
        , expect = Http.expectJson MsgReceived userDecoder
        }

type Msg
    = MsgReceived (Result Http.Error User)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        MsgReceived (Ok user) ->
            ({ model | user = Just user }, Cmd.none)

        MsgReceived (Err _) ->
            (model, Cmd.none)
```

सैम्पल आउटपुट:
```
User { name = "Alice", age = 42 }
```

## Deep Dive (गहराई से जानकारी):
एल्म में HTTP अनुरोध भेजने की प्रक्रिया साफ और निर्भर है। यह एक प्योर फंक्शनल प्रोग्रामिंग भाषा है, जिसका मतलब है कि साइड इफेक्ट्स, जैसे HTTP अनुरोध, को `Cmd` के माध्यम से manage किया जाता है। जावास्क्रिप्ट या अन्य भाषाओं की तरह Promise या Callback का इस्तेमाल नहीं होता।

अतीत में, Ajax और XMLHttpRequest जैसे विभिन्न तरीके थे जिसके द्वारा वेब पर HTTP अनुरोध किए जाते थे। एल्म ने इसे अधिक सरल और अनुमानित बनाया है। डेटा को decode करने के लिए यह `Json.Decode` सिस्टम का उपयोग करता है, जो कि टाइप सुरक्षित है और रनटाइम एरर को कम करता है।

HTTP पैकेज Elm 0.18 में जोड़ा गया था, और इसे Elm 0.19 में सुधारा गया। इसने एल्म एप्लिकेशन्स में HTTP इंटरेक्शन को और भी शक्तिशाली और लचीला बनाया।

## See Also (और देखें):
- [Elm HTTP package documentation](https://package.elm-lang.org/packages/elm/http/latest/)
- [Elm JSON Decode guide](https://guide.elm-lang.org/effects/json.html)
- [Using Elm in Production](https://elm-lang.org/blog/small-assets-without-the-headache)
