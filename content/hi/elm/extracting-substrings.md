---
title:                "सबस्ट्रिंग हटाना"
html_title:           "Elm: सबस्ट्रिंग हटाना"
simple_title:         "सबस्ट्रिंग हटाना"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/elm/extracting-substrings.md"
---

{{< edit_this_page >}}

## इसक्त्यार क्या है?
इस्तेरींग सबस्ट्रिंगस को निकालना क्या है? आमतोर पर, प्रोग्रामर सबस्ट्रिंग को सूची में से निकालते हैं ताकि केवल वह अंश ही दिखाई दे जो उनको आवश्यक हो।

इसके लिए वे string.slice जैसे फंक्शंन्स का उपयोग कर सकते हैं।

## कैसे करें:
```Elm
String.slice 3 6 "Hello, World!" -- "lo,"
```

इस उदाहरण में, हमने "Hello, World!" स्ट्रिंग से अंश "lo," को निकाला है। यहाँ, 3 इंडेक्स को दिया गया है जो दूसरे "l" के बाद आता है और 6 शुरुआत स्थान होता है।

## डीप डाइव:
इस तरह के तरीकों का प्रयोग पहले से ही कई प्रोग्रामिंग लैंग्वेज में किया जाता था। यह एक प्रोग्रामिंग मेथड है जो आर्गुमेंट्स के आधार पर स्ट्रिंग को निकालता है। अन्य विकल्पों में, आप स्ट्रिंगों के भीतर के अनुमतियां तय कर सकते हैं। इसके अलावा, आप अंतरिक्षों और क्रोनोमेटर्स भी इस्तेमाल कर सकते हैं ताकि आप अपने निर्धारित समय के भीतर स्ट्रिंग को निकल सके।

## इससे जुड़ी और पढ़ें:
- [Elm डॉक्युमेन्टेशन](https://guide.elm-lang.org/core_language.html#strings)
- [FreeCodeCamp - Elm for beginners](https://www.freecodecamp.org/news/a-beginners-guide-to-elm/)
- [Elm खुली स्रोत समुदाय](https://elm-lang.org/community)