---
title:                "स्ट्रिंग्स को निकालना"
html_title:           "Javascript: स्ट्रिंग्स को निकालना"
simple_title:         "स्ट्रिंग्स को निकालना"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/javascript/extracting-substrings.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
जब हम किसी बड़े स्ट्रिंग से छोटे सबस्ट्रिंग को अलग करना चाहते हैं, तो हम substring extraction करते हैं। इससे हमें बड़ी स्ट्रिंग का उपयोग केवल उस भाग के लिए करना पड़ता है जिसकी हमें जरूरत होती है। इसलिए programmers ऐसा करते हैं।

## कैसे करें:
चलो जानते हैं कि हम substring extraction किस तरह से कर सकते हैं।

```Javascript
// स्ट्रिंग के दूसरे वर्ग को अलग करना
let str = "मेरा नाम राहुल है";
let sub = str.substring(8,13); // Output: राहुल

// स्ट्रिंग के आखिर में से छोटे सबस्ट्रिंग को अलग करना
let str = "मेरा नाम राहुल है";
let sub = str.substring(13); // Output: है
```

## विस्तृत जानकारी:
1. सबस्ट्रिंग एक्सट्रैक्शन का इतिहास: substring extraction प्रोग्रामिंग की दुनिया में बहुत पुराने समय से है। पहले, यह एक आइडिया था जहां बड़े टेक्स्ट स्ट्रिंग में से कुछ चीज़ें पता लगाने के लिए उपयोग किया जाता था। लेकिन आजकल, substring extraction को प्रोग्रामिंग में डेटा को मैनिप्युलेट करने के लिए उपयोग किया जाता है। 
2. विकल्प: substring extraction का एक और विकल्प हो सकता है string slicing। इसमें अभी भी एक स्ट्रिंग से क्षैतिज सबस्ट्रिंग को अलग करने के लिए हम substr() फ़ंक्शन का उपयोग कर सकते हैं। 
3. प्रोग्रामिंग विवरण: substring extraction एक और कामियाँ है string manipulation की विशेष विधि। हम substr() या substring() फ़ंक्शन के साथ शुरूआत और अंत वर्ग की सहायता से उनसे उपयोगकर्ता की जरूरत के अनुसार उपयोग कर सकते हैं।

## और देखो:
substring extraction से जुड़े और जानकारी प्राप्त करने के लिए नीचे दिए गए स्रोतों को देखें:
- [MDN Web Docs](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/substring)
- [GeeksforGeeks](https://www.geeksforgeeks.org/javascript-substring-method/)