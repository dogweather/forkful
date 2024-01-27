---
title:                "टेक्स्ट फाइल लिखना"
date:                  2024-01-19
html_title:           "Bash: टेक्स्ट फाइल लिखना"
simple_title:         "टेक्स्ट फाइल लिखना"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/elm/writing-a-text-file.md"
---

{{< edit_this_page >}}

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
