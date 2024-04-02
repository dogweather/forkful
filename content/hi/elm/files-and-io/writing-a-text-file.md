---
date: 2024-01-19
description: "\u091F\u0947\u0915\u094D\u0938\u094D\u091F \u092B\u093E\u0907\u0932\
  \ \u0932\u093F\u0916\u0928\u093E \u092E\u0924\u0932\u092C \u0939\u0948 \u0921\u0947\
  \u091F\u093E \u0915\u094B \u091F\u0947\u0915\u094D\u0938\u091F \u092B\u0949\u0930\
  \u094D\u092E\u0947\u091F \u092E\u0947\u0902 \u0938\u0939\u0947\u091C\u0928\u093E\
  \u0964 \u092A\u094D\u0930\u094B\u0917\u094D\u0930\u093E\u092E\u0930\u094D\u0938\
  \ \u092F\u0939 \u0907\u0938\u0932\u093F\u090F \u0915\u0930\u0924\u0947 \u0939\u0948\
  \u0902 \u0915\u094D\u092F\u094B\u0902\u0915\u093F \u0907\u0938\u0947 \u092A\u095D\
  \u0928\u093E \u0914\u0930 \u0938\u0902\u0936\u094B\u0927\u093F\u0924 \u0915\u0930\
  \u0928\u093E \u0906\u0938\u093E\u0928 \u0939\u094B\u0924\u093E \u0939\u0948, \u0938\
  \u093E\u0925 \u0939\u0940 \u0921\u0947\u091F\u093E\u2026"
lastmod: '2024-03-13T22:44:52.218247-06:00'
model: unknown
summary: "\u091F\u0947\u0915\u094D\u0938\u094D\u091F \u092B\u093E\u0907\u0932 \u0932\
  \u093F\u0916\u0928\u093E \u092E\u0924\u0932\u092C \u0939\u0948 \u0921\u0947\u091F\
  \u093E \u0915\u094B \u091F\u0947\u0915\u094D\u0938\u091F \u092B\u0949\u0930\u094D\
  \u092E\u0947\u091F \u092E\u0947\u0902 \u0938\u0939\u0947\u091C\u0928\u093E\u0964\
  \ \u092A\u094D\u0930\u094B\u0917\u094D\u0930\u093E\u092E\u0930\u094D\u0938 \u092F\
  \u0939 \u0907\u0938\u0932\u093F\u090F \u0915\u0930\u0924\u0947 \u0939\u0948\u0902\
  \ \u0915\u094D\u092F\u094B\u0902\u0915\u093F \u0907\u0938\u0947 \u092A\u095D\u0928\
  \u093E \u0914\u0930 \u0938\u0902\u0936\u094B\u0927\u093F\u0924 \u0915\u0930\u0928\
  \u093E \u0906\u0938\u093E\u0928 \u0939\u094B\u0924\u093E \u0939\u0948, \u0938\u093E\
  \u0925 \u0939\u0940 \u0921\u0947\u091F\u093E\u2026"
title: "\u091F\u0947\u0915\u094D\u0938\u094D\u091F \u092B\u093E\u0907\u0932 \u0932\
  \u093F\u0916\u0928\u093E"
weight: 24
---

## What & Why? (क्या और क्यों?)
टेक्स्ट फाइल लिखना मतलब है डेटा को टेक्सट फॉर्मेट में सहेजना। प्रोग्रामर्स यह इसलिए करते हैं क्योंकि इसे पढ़ना और संशोधित करना आसान होता है, साथ ही डेटा एक्सचेंज करने का यह एक सरल माध्यम है।

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
