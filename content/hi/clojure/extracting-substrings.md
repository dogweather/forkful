---
title:                "सबस्ट्रिंग्स निकालना"
html_title:           "Clojure: सबस्ट्रिंग्स निकालना"
simple_title:         "सबस्ट्रिंग्स निकालना"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/clojure/extracting-substrings.md"
---

{{< edit_this_page >}}

## क्या और क्यों? 

Substring का अर्थ होता है किसी स्ट्रिंग का हिस्सा। प्रोग्राञमर्स इसे इसलिए निकालते हैं ताकि वह कुछ विशिष्ट डेटा को उसके मुख्य स्ट्रिंग से अलग कर सकें। 

## कैसे करें:

Clojure में substring निकालने के लिए आप `subs` का इस्तेमाल कर सकते हैं। 

```Clojure
(def str "Hello, World!")
(subs str 0 5)
```

उपर्युक्त कोड में, `subs` को 2 तर्क दिये गए हैं। पहला स्थान जहां से उपनिर्देष शुरू होगा, और दूसरा वह स्थान है जहां उपनिर्देष समाप्त होगा। इसका आउटपुट यह होगा:

```Clojure
"Hello"
```

## गहराई से विश्लेषण:

(1) ऐतिहासिक प्रसंग: Clojure, 2007 में रिच हिकी द्वारा विकसित एक सामर्थ्यवान और माइक्रोसॉफ़्ट .NET प्लेटफॉर्म पर व्याप्य भाषा है। सबस्ट्रिंग मेथड का उपयोग करके डेटा मानिपुलेशन को आसान बनाया गया है। 

(2) विकल्प: `subs` के अलावा, आप `clojure.string/split` का उपयोग भी कर सकते हैं जो आपको एक सूची देगा जिसमें स्ट्रिंग के हर किरण का उपनिर्देष होगा। 

(3) कार्यान्वयन विवरण: `subs` फ़ंक्शन अंतर्निहित Java फ़ंक्शन का उपयोग करता हैं, `substring`, जो दिए गए निर्देशांकों के बीचका हिस्सा निकालता हैं। 

## अधिक देखें:

- Clojure आधिकारिक प्रलेखन: [https://clojure.org/](https://clojure.org/)
- Clojure कोर स्ट्रिंग API: [https://clojuredocs.org/clojure.core](https://clojuredocs.org/clojure.core)
- स्ट्रिंगों के साथ काम करने के लिए Clojure ट्यूटोरियल: [https://www3.ntu.edu.sg/home/ehchua/programming/java/images/javaString.png](https://www3.ntu.edu.sg/home/ehchua/programming/java/images/javaString.png)