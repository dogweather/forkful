---
date: 2024-01-20 18:00:00.788979-07:00
description: "How to: (\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902:) ."
lastmod: '2024-03-13T22:44:52.181141-06:00'
model: gpt-4-1106-preview
summary: .
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
