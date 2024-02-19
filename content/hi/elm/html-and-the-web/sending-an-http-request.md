---
aliases:
- /hi/elm/sending-an-http-request/
date: 2024-01-20 18:00:00.788979-07:00
description: "HTTP \u0905\u0928\u0941\u0930\u094B\u0927 \u090F\u0915 \u092A\u094D\u0930\
  \u0915\u093E\u0930 \u0915\u093E \u0905\u0928\u0941\u0930\u094B\u0927 \u0939\u0948\
  \ \u091C\u093F\u0938\u0947 \u0935\u0947\u092C\u0938\u0930\u094D\u0935\u0930 \u092A\
  \u0930 \u092D\u0947\u091C\u093E \u091C\u093E\u0924\u093E \u0939\u0948 \u0924\u093E\
  \u0915\u093F \u0921\u0947\u091F\u093E \u0932\u093F\u092F\u093E \u092F\u093E \u092D\
  \u0947\u091C\u093E \u091C\u093E \u0938\u0915\u0947\u0964 \u092A\u094D\u0930\u094B\
  \u0917\u094D\u0930\u093E\u092E\u0930 \u0907\u0938\u0947 \u0909\u092A\u092F\u094B\
  \u0917\u0915\u0930\u094D\u0924\u093E\u0913\u0902 \u0915\u094B \u091C\u0930\u0942\
  \u0930\u0940 \u091C\u093E\u0928\u0915\u093E\u0930\u0940 \u0926\u0947\u0928\u0947\
  \ \u092F\u093E \u0921\u093E\u091F\u093E\u2026"
lastmod: 2024-02-18 23:09:03.186210
model: gpt-4-1106-preview
summary: "HTTP \u0905\u0928\u0941\u0930\u094B\u0927 \u090F\u0915 \u092A\u094D\u0930\
  \u0915\u093E\u0930 \u0915\u093E \u0905\u0928\u0941\u0930\u094B\u0927 \u0939\u0948\
  \ \u091C\u093F\u0938\u0947 \u0935\u0947\u092C\u0938\u0930\u094D\u0935\u0930 \u092A\
  \u0930 \u092D\u0947\u091C\u093E \u091C\u093E\u0924\u093E \u0939\u0948 \u0924\u093E\
  \u0915\u093F \u0921\u0947\u091F\u093E \u0932\u093F\u092F\u093E \u092F\u093E \u092D\
  \u0947\u091C\u093E \u091C\u093E \u0938\u0915\u0947\u0964 \u092A\u094D\u0930\u094B\
  \u0917\u094D\u0930\u093E\u092E\u0930 \u0907\u0938\u0947 \u0909\u092A\u092F\u094B\
  \u0917\u0915\u0930\u094D\u0924\u093E\u0913\u0902 \u0915\u094B \u091C\u0930\u0942\
  \u0930\u0940 \u091C\u093E\u0928\u0915\u093E\u0930\u0940 \u0926\u0947\u0928\u0947\
  \ \u092F\u093E \u0921\u093E\u091F\u093E\u2026"
title: "HTTP \u0905\u0928\u0941\u0930\u094B\u0927 \u092D\u0947\u091C\u0928\u093E"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)

HTTP अनुरोध एक प्रकार का अनुरोध है जिसे वेबसर्वर पर भेजा जाता है ताकि डेटा लिया या भेजा जा सके। प्रोग्रामर इसे उपयोगकर्ताओं को जरूरी जानकारी देने या डाटा कलेक्ट करने के लिए करते हैं।

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
