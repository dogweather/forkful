---
title:                "एक http अनुरोध भेजना"
html_title:           "Elm: एक http अनुरोध भेजना"
simple_title:         "एक http अनुरोध भेजना"
programming_language: "Elm"
category:             "Elm"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/elm/sending-an-http-request.md"
---

{{< edit_this_page >}}

अभी तक, आपने शायद एल्म (Elm) से संबंधित कुछ बेसिक जानकारी ही प्राप्त की होगी। यह एक फ़ार्मल लैंग्वेज है जो वेब डेवलपमेंट के लिए उपयोग किया जाता है। इस लेख में, हम एल्म में HTTP रिक्वेस्ट (HTTP request) भेजने के बारे में बात करेंगे और उसे कैसे किया जाता है।

## क्या और क्यों? 
एक HTTP रिक्वेस्ट भेजना एक विशिष्ट प्रकार का डेटा भेजना होता है, जो आपके वेब एप्लिकेशन को सर्वर से डेटा लाने या भेजने की अनुमति देता है। यह डेटा हमेशा कुछ कीमतों या डेटा का एक ढांचा होता है जो आप अपने कोड में उपयोग कर सकते हैं। वेब डेवलपमेंट में, HTTP रिक्वेस्ट भेजना बहुत आम होता है और यह वेब एप्लिकेशन को डायनामिक बनाने में मदद करता है।

## कैसे करें? 
एल्म में HTTP रिक्वेस्ट भेजने के लिए हम Http.send फ़ंक्शन का उपयोग कर सकते हैं। नीचे दिए गए कोड ब्लॉक देखें:

```
import Http
import Json.Decode exposing (int, string, Decoder)

type Msg = Success Int | Failure String

reqMethod : Http.Method
reqMethod =
  Http.get

reqUrl : String
reqUrl =
  "https://www.example.com"

reqDecoder : Decoder Int
reqDecoder =
  int

sendReq : Msg -> Cmd Msg
sendReq msg =
  Http.send (\response -> 
    case response of
      Http.BadUrl err ->
        Failure err
      Http.Timeout ->
        Failure "Request timed out"
      Http.NetworkError ->
        Failure "Network error"
      Http.BadStatus status ->
        Failure (String.fromInt status)
      Http.GoodStatus body ->
        case Json.Decode.decodeString reqDecoder body of
          Ok value ->
            Success value
          Err err ->
            Failure err
    ) (Http.request
          { method = reqMethod
          , headers = []
          , url = reqUrl
          , body = Http.emptyBody
          , expect = Http.expectString reqDecoder })
```

यहां, Http.send कोड ब्लॉक में दिए गए टाइप के साथ प्रोटोकॉल के अनुसार काम करता है। हमने Http.request को उसी प्रोटोकॉल के अनुसार सेटअप किया है जिसे विश्वास योग्य जवाब मिलता दिखाई देता है।

## गहराई में जाएं 
HTTP रिक्वेस्टें वेब डेवलपमेंट का एक अहम हिस्सा हैं और वे सर्वर से विश्वास योग्य जवाब लाने में मदद करते हैं। ऐतिहासिक पृष्ठ से, HTTP रिक्वेस्टें थर्ड पार्टी क्लाइंट साइड से सर्वर को डेटा भेजने का माध्यम बने हुए हैं। यदि आप Http.send के बजाय Native modules उपयोग करना चाहते हैं, तो Elm दुनिया में कुछ अन्य अल्टरनेटिव्स भी हैं। इसके अलावा, आप मल्टी-प्लेटफार्म एप्स बनाने के लिए Elm Native UI भी उपयोग कर सकते हैं।

## उन्नत जानकारी 
यदि आप अभी भी HTTP रिक्वेस्ट को और गहर