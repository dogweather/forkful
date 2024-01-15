---
title:                "एक http निवेदन भेजना"
html_title:           "Elm: एक http निवेदन भेजना"
simple_title:         "एक http निवेदन भेजना"
programming_language: "Elm"
category:             "Elm"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/elm/sending-an-http-request.md"
---

{{< edit_this_page >}}

## क्यों

ताकि वे किसी वेब साइट से डेटा को अपने ऐप्लिकेशन में दिखाने के लिए, उपयोगकर्ता को अपने सर्वर से डेटा को प्राप्त करने के लिए एचटीटीपी अनुरोध भेजे जा सकते हैं।

## कैसे करें

एल्म में एचटीटीपी अनुरोध कैसे भेजा जाता है, उसे उदाहरण के साथ समझाने के लिए हमें निम्न सेक्शन में दिखाएंगे।

हम पहले एक प्रोजेक्ट शुरू करते हैं। जब आप यह क्रिया पूर्ण कर लेंगे, आपको निम्न तरह की स्ट्रक्चर मिलेगी।

```Elm
import Http
import Json.Encode

type Msg = DataResponse (Result String String) | Error String

type alias Data = 
    { name : String
    , age : Int
    }

url : String
url = "https://randomuser.me/api/"

dataRequest : Http.Request Data
dataRequest =
    Http.get 
        { url = url
        , expect = Http.expectJson DataResponse
        }

```

ऊपर दिए गए कोड ब्लॉक में, हमने `Http` और `Json.Encode` मॉड्यूल को आयात किया है और `Msg` और `Data` नाम के टाइप को डिफाइन किया है। हमने `DataResponse` और `Error` नाम के कस्टम डेटा टाइप भी बनाया है। आगे हम `url` और `dataRequest` कंपोसीशन टाइप भी बनाएंगे।

अब हम `update` फंक्शन बनाते हैं और एचटीटीपी अनुरोध को भेजते हैं।

```Elm
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DataResponse (Ok data) ->
            ( { model | name = data.name, age = data.age }, Cmd.none )
        DataResponse (Err error) ->
            ( { model | name = "Error", age = 0 }, Cmd.none )
        Error error ->
            ( { model | name = "Error", age = 0 }, Cmd.none )

```

ऊपर दिए गए कोड ब्लॉक में, हम `DataResponse` और `Error` मैसेज पर कंडिशनल लॉजिक रखते हैं और डेटा को पारसर करते हैं। अगर डेटा सफलतापूर्वक प्राप्त होता है, तो हम डेटा को मॉडल में सेट करते