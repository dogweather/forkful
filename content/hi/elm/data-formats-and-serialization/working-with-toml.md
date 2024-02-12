---
title:                "TOML के साथ काम करना"
aliases: - /hi/elm/working-with-toml.md
date:                  2024-01-26T04:23:03.953930-07:00
model:                 gpt-4-0125-preview
simple_title:         "TOML के साथ काम करना"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/elm/working-with-toml.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
TOML, जिसका पूरा नाम Tom's Obvious, Minimal Language है, एक डेटा सीरियलाइजेशन भाषा है। Elm प्रोग्रामर इसका इस्तेमाल कॉन्फ़िगरेशन डेटा को प्रबंधित करने के लिए करते हैं क्योंकि यह मानव-पठनीय है और एप्लिकेशनों में आवश्यक की-वैल्यू जोड़ियों को साफ़-सुथरा मैप करता है।

## कैसे:
Elm में एक निर्मित TOML पार्सर नहीं है, लेकिन आप जावास्क्रिप्ट के साथ इंटरॉप कर सकते हैं या एक सामुदायिक पैकेज का उपयोग कर सकते हैं। यहाँ एक काल्पनिक `elm-toml` पैकेज का उपयोग करके TOML को पार्स करने का तरीका है:

```elm
आयात Toml

configToml : स्ट्रिंग
configToml =
    """
    [server]
    port = 8080
    """

parseResult : परिणाम Toml.Decode.Error Toml.Value
parseResult =
    Toml.decodeString configToml
```

विशिष्ट मानों को डिकोड करने के लिए:

```elm
portDecoder : Toml.Decode.Decoder Int
portDecoder =
    Toml.Decode.field "server" (Toml.Decode.field "port" Toml.Decode.int)

port : परिणाम स्ट्रिंग Int
port =
    Toml.decodeString portDecoder configToml
```

`port` के लिए नमूना आउटपुट `Ok 8080` हो सकता है अगर डिकोडिंग सफल हो।

## गहराई में
TOML को GitHub के सह-संस्थापक टॉम प्रेस्टन-वर्नर ने कॉन्फ़िगरेशन फाइलों के लिए एक सरल भाषा के रूप में बनाया था। यह YAML और JSON के साथ प्रतिस्पर्धा करता है; TOML का सिंटैक्स दोनों दुनियाओं की सर्वोत्तम विशेषताओं के लिए लक्ष्य बनाता है जिसमें मानव द्वारा पढ़ने और लिखने में आसानी पर ध्यान केंद्रित होता है।

Elm में TOML से निपटने के लिए, आपको आमतौर पर जावास्क्रिप्ट इंटरॉप के माध्यम से जाना पड़ता है, जो थोड़ा कठिन हो सकता है। सौभाग्य से, Elm समुदाय संसाधन समृद्ध है, और कई तृतीय-पक्ष पैकेज मौजूद हैं। एक काल्पनिक `elm-toml` पैकेज शायद Elm के `Port` का उपयोग करके एक जावास्क्रिप्ट TOML पार्सर से बात करेगा या सीधे Elm में पार्सिंग को लागू करेगा।

Elm में मुख्य बाधा यह है कि यह सब कुछ स्टेटिकली टाइप करता है, इसलिए आपको TOML के भीतर विभिन्न डेटा संरचनाओं को संभालने के लिए कस्टम डिकोडर लिखने की आवश्यकता होती है, जो थोड़ा शब्दाडंबरपूर्ण हो सकता है लेकिन सुरक्षा जोड़ता है।

## देखें भी
TOML स्वयं पर विनिर्देशों और अधिक जानकारी के लिए, [TOML](https://toml.io) देखें।
यदि आप Elm और जावास्क्रिप्ट इंटरॉप के हाथों-हाथ दृष्टिकोण की तलाश कर रहे हैं, तो आधिकारिक मार्गदर्शिका के साथ शुरू करें: [Elm Ports](https://guide.elm-lang.org/interop/ports.html)।
सामुदायिक पैकेजों के लिए या यो CONTRIBUTOR करने के लिए, [Elm Packages](https://package.elm-lang.org/) ब्राउज़ करें।
