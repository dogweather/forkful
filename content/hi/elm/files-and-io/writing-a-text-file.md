---
date: 2024-01-19
description: "How to: (\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902:) Elm \u092E\
  \u0947\u0902 \u0938\u0940\u0927\u0947 \u091F\u0947\u0915\u094D\u0938\u094D\u091F\
  \ \u092B\u093E\u0907\u0932 \u0928\u0939\u0940\u0902 \u0932\u093F\u0916 \u0938\u0915\
  \u0924\u0947 \u0915\u094D\u092F\u094B\u0902\u0915\u093F \u092F\u0939 \u090F\u0915\
  \ \u0935\u0947\u092C-\u092B\u094D\u0930\u0902\u091F\u090F\u0902\u0921 \u092D\u093E\
  \u0937\u093E \u0939\u0948\u0964 \u0932\u0947\u0915\u093F\u0928, \u0939\u092E \u090F\
  \u0915 HTTP \u0905\u0928\u0941\u0930\u094B\u0927 \u092C\u0928\u093E \u0938\u0915\
  \u0924\u0947 \u0939\u0948\u0902 \u091C\u093F\u0938\u0947 \u092C\u0948\u0915-\u090F\
  \u0902\u0921 \u0938\u0930\u094D\u0935\u0930\u2026"
lastmod: '2024-04-05T21:53:54.226835-06:00'
model: unknown
summary: "(\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902:) Elm \u092E\u0947\u0902\
  \ \u0938\u0940\u0927\u0947 \u091F\u0947\u0915\u094D\u0938\u094D\u091F \u092B\u093E\
  \u0907\u0932 \u0928\u0939\u0940\u0902 \u0932\u093F\u0916 \u0938\u0915\u0924\u0947\
  \ \u0915\u094D\u092F\u094B\u0902\u0915\u093F \u092F\u0939 \u090F\u0915 \u0935\u0947\
  \u092C-\u092B\u094D\u0930\u0902\u091F\u090F\u0902\u0921 \u092D\u093E\u0937\u093E\
  \ \u0939\u0948\u0964 \u0932\u0947\u0915\u093F\u0928, \u0939\u092E \u090F\u0915 HTTP\
  \ \u0905\u0928\u0941\u0930\u094B\u0927 \u092C\u0928\u093E \u0938\u0915\u0924\u0947\
  \ \u0939\u0948\u0902 \u091C\u093F\u0938\u0947 \u092C\u0948\u0915-\u090F\u0902\u0921\
  \ \u0938\u0930\u094D\u0935\u0930 \u0939\u0948\u0902\u0921\u0932 \u0915\u0930 \u0938\
  \u0915\u0924\u093E \u0939\u0948\u0964 \u092F\u0939\u093E\u0901 \u090F\u0915 \u0909\
  \u0926\u093E\u0939\u0930\u0923 \u0939\u0948."
title: "\u091F\u0947\u0915\u094D\u0938\u094D\u091F \u092B\u093E\u0907\u0932 \u0932\
  \u093F\u0916\u0928\u093E"
weight: 24
---

## How to: (कैसे करें:)
Elm में सीधे टेक्स्ट फाइल नहीं लिख सकते क्योंकि यह एक वेब-फ्रंटएंड भाषा है। लेकिन, हम एक HTTP अनुरोध बना सकते हैं जिसे बैक-एंड सर्वर हैंडल कर सकता है। यहाँ एक उदाहरण है:

```Elm
module Main exposing (..)

import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Http

type Msg = SaveFile

type alias Model = ()

initialModel : Model
initialModel = ()

saveFile : Msg -> Cmd Msg
saveFile SaveFile =
    Http.post
        { url = "http://example.com/save"
        , body = Http.stringBody "text/plain" "यह मेरी फाइल का टेक्स्ट है"
        , expect = Http.expectWhatever (const SaveFile)
        }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        SaveFile ->
            (model, saveFile SaveFile)

view : Model -> Html Msg
view model =
    div []
        [ button [ onClick SaveFile ] [ text "फाइल सहेजें" ]
        ]

main =
    Browser.sandbox { init = initialModel, update = update, view = view }
```

इस कोड को चलाने पर आपको बटन मिलेगा। उसे क्लिक करने पर, यह "यह मेरी फाइल का टेक्स्ट है" टेक्स्ट को सर्वर पर पोस्ट करेगा।

## Deep Dive (गहराई में जानकारी)
Elm में डायरेक्ट फाइल ऑपरेशन्स नहीं हैं क्योंकि यह ब्राउज़र पर चलता है, और ब्राउज़र सुरक्षा कारणों से इसे अनुमति नहीं देते। हालांकि, JavaScript के साथ Elm को इंटरऑप कर ये संभव हो सकता है। इतिहास में देखें तो Elm 0.19 अपडेट के बाद से मौजूदा इंटरऑप सीमित हो गया है, पर एचटीटीपी और कस्टम ईवेंट्स के ज़रिए बहुत कुछ किया जा सकता है।

## See Also (देखें ये भी)
- [Elm Documentation](https://guide.elm-lang.org/)
- [Handling Files with JavaScript and Elm](https://medium.com/@_rchaves_/handling-files-with-javascript-and-elm-9cfb195451e5)
