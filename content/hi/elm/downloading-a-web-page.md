---
title:                "वेब पेज डाउनलोड करना"
html_title:           "Elm: वेब पेज डाउनलोड करना"
simple_title:         "वेब पेज डाउनलोड करना"
programming_language: "Elm"
category:             "Elm"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/elm/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## क्यूँ
वेब पेज को डाउनलोड करना किसी भी प्रोग्रामर के लिए एक अहम होता है। यह हमें अपने कोड में अन्य पेज के साथ संवाद करने की अनुमति देता है और विभिन्न अनुरोधों का जवाब देता है।

## कैसे करें
वेब पेज को डाउनलोड करने के लिए आप ईल्‍म का उपयोग कर सकते हैं। आप `Http` मोड्यूल का उपयोग करके वेब पेज को GET या POST अनुरोधों के माध्यम से डाउनलोड कर सकते हैं। यहाँ एक उदाहरण है:

```Elm
import Http
import Json.Decode exposing (..)

type Msg
    = WebPageLoaded (Result Http.Error String)

getWebPage : Cmd Msg
getWebPage =
    Http.get
        { url = "https://example.com"
        , expect = Http.expectString WebPageLoaded
        }
```

उपरोक्त उदाहरण में, हमने `Http` मोड्यूल को आयात किया है और एक `Msg` डेटा टाइप बनाया है जो WebPageLoaded नामक एक भेजने वाले संदेश को संदर्भित करता है। फिर, हम `Http.get` फ़ंक्शन का उपयोग करके एक GET अनुरोध भेजते हैं और उससे प्राप्त हुए डेटा को `expectString` फ़ंक्शन के माध्यम से String के रूप में पार्स करते हैं।

## गहराई में धावन
वेब पेज को डाउनलोड करने के लिए ईल्‍म में और अधिक विकसित तरीकों का भी उपलब्ध है। आप `Task` मोड्यूल का उपयोग करके पैरालल प्रसंस्करण कर सकते हैं, किसी अन्य पेज पर पॉलिंग कर सकते हैं या कोई और निर्धारित तरीके से डाउनलोड कर सकते हैं। और, आप अपने प्रोग्राम को और बेहतर तरीके से प्रबंधित करने के लिए प्रियंकाए और कन्फ़िगरेशन का उपयोग