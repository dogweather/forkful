---
title:                "सबस्ट्रिंग्स निकालना"
html_title:           "Clojure: सबस्ट्रिंग्स निकालना"
simple_title:         "सबस्ट्रिंग्स निकालना"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/elm/extracting-substrings.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

उपस्ट्रिंग्स निकालना का मतलब होता है किसी विशेष विशेष धागे का हिस्सा चुनना। प्रोग्रामर्स इसका उपयोग डेटा संसाधन या उपयोगकर्ता इनपुट को विश्लेषित करने के लिए करते हैं।

## कैसे करें:

Elm में, आप `String.slice` फ़ंक्शन का उपयोग करके उपवर्णमाला प्राप्त कर सकते हैं। इसका एक उदाहरण नीचे दिया गया है:

```Elm
import String

main =
    let
        str = "यहाँ हिंदी में एक उदाहरण है"
        start = 5
        end = 9
    in
    String.slice start end str
```

यह उदाहरण `" हिंदी "` आउटपुट देगा। `String.slice` फ़ंक्शन का पहला प्रारम्भिक बिंदु है, दूसरा अंतिम बिंदु है, और तीसरा स्ट्रिंग है जिसमें से हम उपस्ट्रिंग निकालना चाहते हैं। 

## गहराई डाइव:

तात्कालिक संदर्भ में, Elm प्रोग्रामिंग भाषा में `String.slice` विधि उपस्ट्रिंग्स निकालने की सर्वाधिक प्रयुक्त विधि है। इसे Elm की कोर लाइब्रेरी में पेश किया गया है और यह किसी भी विपणन में उपलब्ध होता है। बदले में, विभिन्न अन्य विधियाँ भी हैं जैसे कि `String.left` और `String.right`, लेकिन ये समग्र स्ट्रिंग के विपरीत ओर से सीमित होते हैं।

यदि आप दीर्घाकार उपस्ट्रिंग निकालने की कोशिश करते हैं जो मूल स्ट्रिंग से अधिक है, तो `String.slice` केवल मूल स्ट्रिंग को ही वापस लौटाएगा। 

## अन्य संसाधन:

अधिक जानकारी के लिए, आप निम्न संसाधनों का उपयोग कर सकते हैं:

1. [Elm String लाइब्रेरी डॉक्यूमेंटेशन](https://package.elm-lang.org/packages/elm/core/latest/String)
2. [Elm के साथ प्रोग्रामिंग](https://elm-lang.org/docs)
3. [String.slice()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/slice) JavaScript के एक समरूप फ़ंक्शन के विवरण. Elm और JavaScript के फंक्शन का व्यवहार समान है।