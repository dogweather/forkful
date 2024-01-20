---
title:                "http अनुरोध भेजना"
html_title:           "Elixir: http अनुरोध भेजना"
simple_title:         "http अनुरोध भेजना"
programming_language: "Elm"
category:             "Elm"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/elm/sending-an-http-request.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

HTTP अनुरोध भेजना यह सुनिश्चित करने का एक तरीका है कि वेब ब्राउज़र (या किसी अन्य क्लाइंट) सर्वर से डेटा भेज सके और प्राप्त कर सके। प्रोग्रामर इसे तब करते हैं जब उन्हें वेब क्लाइंट और सर्वर के बीच डेटा संवाद की आवश्यकता होती है।

## कैसे करें:

निम्नलिखित कोड एक HTTP GET request उदाहरण है:

```Elm
import Http exposing (..)
import Json.Decode as Decode

getExample : String -> Cmd Msg
getExample url =
    Http.get
        { url = url
        , expect = Http.expectString GotResult
        }

type Msg =
    GotResult (Result Http.Error String)

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotResult result ->
            case result of
                Ok data ->
                    ( { model | data = Just data }, Cmd.none )

                Err _ ->
                    ( model, getExample "https://jsonplaceholder.typicode.com/posts/1" )
```

सामान्य रूप से, आपका आउटपुट प्राप्त पदों की सूची होगा।

## गहराई में जानें: 

1) HTTP अनुरोध का विकास 1990 में हुआ था, जब वर्ल्ड वाइड वेब को जन्म दिया गया। यह सर्वर के साथ तुरंत संवाद करने की क्षमता प्रदान करता है।
 
2) HTTP के विकल्प में वेबसॉकेट, एजैक्स, और साप्ट शामिल हैं, जो सभी संवेदनशील प्रवाहों के लिए कुछ कोई फायदा प्रदान करते हैं। 

3) Elm में HTTP अनुरोध का निपटारा `Cmd` द्वारा किया जाता है, एक तरीका जो Elm की प्रतिक्रिया सिस्टम के साथ बहुत अच्छी तरह से मिलता है। 

## अन्य स्रोत:

1) [MDN वेब डॉक्स - HTTP परिचय](https://developer.mozilla.org/hi/docs/Web/HTTP/Overview)
2) [Elm प्रलेखन - Http](https://package.elm-lang.org/packages/elm/http/latest/)
3) [Elm Guide - Effects](https://guide.elm-lang.org/effects/)