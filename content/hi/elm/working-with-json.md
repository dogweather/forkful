---
title:                "Elm: जेसन के साथ काम करना"
simple_title:         "जेसन के साथ काम करना"
programming_language: "Elm"
category:             "Elm"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/elm/working-with-json.md"
---

{{< edit_this_page >}}

## क्यों

जिस तरह सामान्य भाषाओं में हम लोग वेबसाइट्स और एप्लिकेशन तैयार करते हैं, वैसे ही वेब विकास में हम अपनी डाटा को जेसन (JSON) फार्मेट में भी स्टोर करते हैं। इससे हम अपने वेबसाइट या एप्लिकेशन को डाइनामिक बना सकते हैं और उसमें जानकारी को आसानी से एक सेवर से दूसरे सेवर पर ट्रांसफर कर सकते हैं।

## कैसे करें

शुरू करने के लिए, हमें `Json.Encode` और `Json.Decode` मॉड्यूल क्रीएट करने की आवश्यकता होगी। फिर, हम अपनी डाटा को `encode` करेंगे और उसे स्ट्रिंग में convert करेंगे। उसके बाद, हम उस स्ट्रिंग को डिकोड करेंगे और हमारी वांछित जानकारी प्राप्त करने के लिए `decode` करेंगे।

```
import Json.Encode as Encode
import Json.Decode as Decode

-- Example data
data : { name : String, age : Int }
data =
    { name = "John", age = 30 }

-- Encoding the data
dataToEncode : Encode.Value
dataToEncode =
    Encode.object
        [ ( "name", Encode.string data.name )
        , ( "age", Encode.int data.age )	
        ]

-- Decoding the data
decodedData : Decode.Decoder ( { name : String, age : Int } )
decodedData =
    Decode.map2 (,)
        (Decode.field "name" Decode.string)
        (Decode.field "age" Decode.int)
```

इसके बाद, हम डेटा को इस तरह उपयोग कर सकते हैं:

```
-- Encoding the data
encodedData : String
encodedData =
    Encode.encode 0 dataToEncode

-- Decoding the data
decodedData : Result String { name : String, age : Int }
decodedData =
    Decode.decodeString decodedData encodedData
```

## गहराई में खोज

जब हम जेसन डेटा को encode और decode करते हैं, तो हम उसमें गहराई से खोज कर सकते हैं। हम उन्हें मैप, फील्ड, और एक्यूल कम्बाइनिंग के द्वारा एक साथ कर सकते हैं, जिससे हमारे पास अधिक उपयोगी जानकारी हो सकती है। इस तरह से हम अपने डेटा को प्रभावी और गुणव