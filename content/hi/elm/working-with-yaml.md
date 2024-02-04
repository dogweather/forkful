---
title:                "YAML के साथ काम करना"
date:                  2024-02-03T19:26:15.351936-07:00
model:                 gpt-4-0125-preview
simple_title:         "YAML के साथ काम करना"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/elm/working-with-yaml.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## क्या और क्यों?

एल्म में YAML के लिए अंतर्निहित समर्थन नहीं है, जो एक डेटा सीरियलाइज़ेशन प्रारूप है जिसे अक्सर कॉन्फ़िगुरेशन फाइलों या डेटा साझाकरण के लिए इस्तेमाल किया जाता है, इसके प्रबल प्रकार सुरक्षा और पूर्वानुमेय परिणामों पर जोर देने के कारण। हालांकि, वेब विकास में एपीआई या कॉन्फ़िगरेशनों से निपटते समय प्रोग्रामर्स अक्सर YAML से सामना करते हैं, जिससे एल्म के कठोर प्रकार प्रणाली में YAML डेटा को पार्स करने के विश्वसनीय तरीकों की आवश्यकता होती है ताकि बिना किसी बाधा के एकीकरण और नियंत्रण संभव हो सके।

## कैसे:

एल्म में YAML से निपटने के लिए, आपको आमतौर पर YAML को एल्म के बाहर JSON में परिवर्तित करने की आवश्यकता होती है और फिर डेटा के साथ काम करने के लिए एल्म की बिल्ट-इन JSON डिकोडर फ़ंक्शनलिटी का उपयोग करते हैं। जबकि इस दृष्टिकोण में एक अतिरिक्त रूपांतरण चरण की आवश्यकता होती है, यह एल्म की मजबूत प्रकार प्रणाली का लाभ उठाते हुए डेटा अखंडता को सुनिश्चित करता है। YAML से JSON में रूपांतरण के लिए लोकप्रिय उपकरणों में ऑनलाइन कन्वर्टर्स या बैकएंड सेवाएं शामिल हैं। एक बार जब आपके पास JSON हो, तो आप डेटा के साथ काम करने के लिए एल्म के `Json.Decode` मॉड्यूल का उपयोग कर सकते हैं।

सबसे पहले, मान लें आपके पास निम्नलिखित YAML डेटा है:

```yaml
person:
  name: Jane Doe
  age: 30
```

इसे JSON प्रारूप में परिवर्तित करें:

```json
{
  "person": {
    "name": "Jane Doe",
    "age": 30
  }
}
```

फिर, अपना एल्म मॉडल और डिकोडर परिभाषित करें:

```elm
module Main exposing (..)

import Html exposing (text)
import Json.Decode as Decode

type alias Person =
    { name : String
    , age : Int
    }

personDecoder : Decode.Decoder Person
personDecoder =
    Decode.map2 Person
        (Decode.field "name" Decode.string)
        (Decode.field "age" Decode.int)

```

इस डिकोडर का उपयोग JSON को एल्म प्रकार में परिवर्तित करने के लिए करें:

```elm
import Json.Decode as Decode

jsonString = 
    """
    {
      "person": {
        "name": "Jane Doe",
        "age": 30
      }
    }
    """

decodeResult = Decode.decodeString (Decode.field "person" personDecoder) jsonString

main =
    case decodeResult of
        Ok person ->
            Html.text ("Hello, " ++ person.name ++ "!")
            
        Err _ ->
            Html.text "डिकोडिंग करते समय एक त्रुटि हुई।"
```

आउटपुट (एल्म एप्लीकेशन में रेंडर किया गया):
```
Hello, Jane Doe!
```

यह दृष्टिकोण सुनिश्चित करता है कि आप एल्म में YAML डेटा के साथ काम कर सकते हैं, JSON को एक मध्यवर्ती प्रारूप के रूप में उपयोग करके, एल्म की मजबूत प्रकार प्रणाली और JSON डिकोडिंग क्षमताओं का लाभ उठाकर बाहरी डेटा को सुरक्षित और प्रभावी ढंग से नियंत्रित करने के लिए।
