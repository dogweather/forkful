---
title:                "json के साथ काम करना"
html_title:           "Elm: json के साथ काम करना"
simple_title:         "json के साथ काम करना"
programming_language: "Elm"
category:             "Elm"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/elm/working-with-json.md"
---

{{< edit_this_page >}}

## क्यों

आपने कभी सोचा है कि अपने एक्सेप्लोरर या स्मार्टफ़ोन पर विभिन्न ऐप्स को चलाते समय कैसे नेटवर्क पर जाते हुए डेटा को बैकग्राउंड में आनंद लिया जाता है? यह सब मॉडर्न वेब डेवलपमेंट के शीर्ष विषयों में से एक है। एपिसी और स्टार पैटर्न के बारे में आते हैं, जहां डेटा दूसरे सर्वर से लोड किया जाता है। जेसोन हमारी जिंदगी में ऐसा ही एक फॉर्मेट है और इसका इस्तेमाल करके हम प्रोग्रामिंग में अपनी सुगमता बढ़ा सकते हैं। इसलिए, इस लेख में हम जेसोन के साथ काम करने के बारे में बात करेंगे।

## कैसे करें

तो आपने जेसोन को बारीकी से समझ लिया है और अब इसका उपयोग करना चाहते हैं। आइए हम इल्म में इसके साथ काम करना सीखते हैं। हम नीचे कुछ उदाहरण देखेंगे जिन्हें अपने संदर्भ से प्रतिबद्ध किया गया है।

```Elm
import Json.Decode exposing (..)

asset : Decoder String
asset =
    field "name" string

decodeAsset : String -> Result String String
decodeAsset data =
    decodeString asset data
        |> Result.mapError (\_ -> "Invalid JSON")
```

जैसा कि आप देख सकते हैं, हमने `Json.Decode` मॉड्यूल इम्पोर्ट किया है, जो हमें डेकोडिंग के लिए आवश्यक फ़ंक्शन प्रदान करता है। यहां हमने `asset` कुंजी शब्द का उपयोग करके एक स्ट्रिंग को डेकोड किया है जो जेसोन ऑब्जेक्ट के `name` फ़ील्ड से आता है। फिर `decodeAsset` फ़ंक्शन का उपयोग करके `Result` ऑब्जेक्ट में