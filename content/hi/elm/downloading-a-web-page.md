---
title:                "एक वेब पेज डाउनलोड करना"
html_title:           "Kotlin: एक वेब पेज डाउनलोड करना"
simple_title:         "एक वेब पेज डाउनलोड करना"
programming_language: "Elm"
category:             "Elm"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/elm/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

वेब पेज को डाउनलोड करना उसकी सामग्री को अपने कंप्यूटर पर डाउनलोड करने का कार्य होता है। प्रोग्रामर इसे करते हैं क्योंकि यह उन्हें डेटा के साथ काम करने में मदद करता हैतौर कंप्यूटर की लोकल कॉपी बनाकर हमें बाद में उसे ऑफ़लाइन पड़ने की सुविधा देता है।

## कैसे करें:

Elm में, आप `Http` पैकेज का उपयोग करके वेब पेज को डाउनलोड कर सकते हैं। नीचे एक सरल उदाहरण दिया गया है:

```Elm

import Html exposing (Html, text)
import Http

main =
  Http.get { url = "http://example.com", expect = Http.expectString GotResponse }

type Msg = GotResponse (Result Http.Error String)

update msg model =
  case msg of
    GotResponse result ->
      case result of
        Ok body -> 
          ( body, Cmd.none )
        
        Err _ -> 
          ( "Something went wrong", Cmd.none )
          
display model =
  Html.div [] [ Html.text model ]

main = 
  Html.beginnerProgram { model = "", view = display, update = update }

```

## गहरी डाइव:

1. **ऐतिहासिक प्रसंग:** वेब पेज डाउनलोडिंग की क्षमता की मांग हमेशा से रही है। एचटीटीपी (HTTP) प्रोटोकॉल ईंटरनेट की मांग की पूर्ति के लिए डिज़ाइन की गई थी।

2. **विकल्प:** `Http` पैकेज के अलावा, आप `elm-fetch` जैसे पैकेज का भी उपयोग कर सकते हैं।

3. **कार्यान्वयन विवरण:** `Http.get` का उपयोग करते समय, आप एक यूआरएलऔर 'Http.expectString' फंक्शन का उपयोग करके एक `Http.Request` बनाते हैं। यह अनुरोध सर्वर को भेजा जाता है और सर्वर की प्रतिक्रिया को `Msg` टाइप के रूप में हैंडल किया जाता है।

## भी देखें:

1. [Elm के आधिकारिक डॉक्यूमेंटेशन](https://guide.elm-lang.org/) 
2. [Elm के साथ HTTP अनुरोध](https://korban.net/posts/elm/2018-11-28-elm-http-request) 
3. [GitHub पर Elm-Http पैकेज](https://github.com/elm/http)