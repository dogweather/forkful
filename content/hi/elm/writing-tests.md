---
title:                "टेस्ट लिखना"
html_title:           "Elm: टेस्ट लिखना"
simple_title:         "टेस्ट लिखना"
programming_language: "Elm"
category:             "Elm"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/elm/writing-tests.md"
---

{{< edit_this_page >}}

## क्यों

नए सॉफ्टवेयर प्रोजेक्ट्स को डेवलप करने के दौरान आम तौर पर हम समय और परिश्रम को उबाऊ बनाते हैं, लेकिन टेस्टिंग बहुत ही जरूरी है क्योंकि यह हमारे प्रोजेक्ट में बग्स और विपरीत नकारात्मकताओं को अपने उर्जा के निर्माण से आगे रखने में मदद करता है। इसलिए, एल्म डेवलपर्स के लिए एल्म इंटरफेस टेस्टिंग (Elm Interface Testing) का समर्थन निश्चित रूप से जरूरी है।

## कैसे करें

एल्म में टेस्ट का निर्माण बहुत ही सरल है। पहले हम `elm-test` टूल को अपने प्रोजेक्ट में इनस्टॉल करते हैं। अब टेस्टिंग करने के लिए `tests` डाइरेक्टरी बनाएं और उसमें `Main.elm` फ़ाइल बनाएं। नीचे एक उदाहरण दिया गया है:

```Elm
module Main exposing (..)

import Html exposing (..)
import Expect exposing (..)

greeting : String
greeting = "Hello, world!"

tests =
    [ test "greeting should be Hello, world!" <|
        \() -> Expect.equal greeting "Hello, world!"
    ]

main =
    Html.program
        { view = \_ -> div [] [ text "Testing in Elm" ]
        , init = ( (), Cmd.none )
        , update = \_ _ -> ( (), Cmd.none )
        , subscriptions = \_ -> Sub.none
        }
```

इसके बाद, हम `elm-test` कमांड रन करें तो निम्न आउटपुट मिलेगा:

```
➜ elm-test

elm-test 0.19.0-rev3
---------------------

Running 1 test. To reproduce these results, run: elm-test --fuzz 100 --seed 269593148

TEST RUN PASSED(1 test)

elm-test by Elm version 0.19.0-rev3
```

## गहराईगत समझ

टेस्ट लेखन हमारे प्रोजेक्ट को सुधारने और दूषित कोड को ढूंढने में मदद करता है। यदि हम एल्म दो आरोही संरचनाएं विभिन्न टेस्ट बनाते हैं जो हमारी परीक्षा फ़ालतू से बचाते हैं तो यह ज्यादा लोकप्र