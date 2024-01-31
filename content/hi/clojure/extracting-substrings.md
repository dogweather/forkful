---
title:                "सबस्ट्रिंग्स निकालना"
date:                  2024-01-20T17:45:21.873201-07:00
model:                 gpt-4-1106-preview
simple_title:         "सबस्ट्रिंग्स निकालना"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/clojure/extracting-substrings.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
Substring एक स्ट्रिंग का हिस्सा होता है। प्रोग्रामर्स इसे बड़ी स्ट्रिंग्स से जानकारी निकालने या प्रोग्राम्स में खास डाटा प्रोसेस करने के लिए उपयोग करते हैं।

## How to: (कैसे करें:)
Clojure में substrings निकालने के लिए `subs` फंक्शन का प्रयोग होता है। नीचे कुछ उदाहरण दिए गए हैं:

```Clojure
;; स्ट्रिंग से पहले 5 characters निकालें
(subs "नमस्ते, Clojure!" 0 5) ; => "नमस्ते"

;; 7वें character से शुरू करके बाकी स्ट्रिंग निकालें
(subs "नमस्ते, Clojure!" 7) ; => "Clojure!"

;; 8वें से 14वें character तक का हिस्सा निकालें
(subs "नमस्ते, Clojure!" 7 14) ; => "Clojur"
```
## Deep Dive (गहराई से जानकारी)
Substring की अवधारणा प्रोग्रामिंग भाषाओं के शुरुआती दिनों से ही है। Clojure में `subs` फंक्शन Java Platform की `substring` मेथड पर आधारित है। वैकल्पिक तौर पर, स्ट्रिंग्स को हैंडल करने के लिए रेगेक्स (regular expressions) का इस्तेमाल भी किया जा सकता है। Clojure के पीछे के इम्प्लिमेंटेशन में, स्ट्रिंग्स immutable होते हैं, इसलिए `subs` फंक्शन कॉल हर बार एक नया स्ट्रिंग बनाती है।

## See Also (इसे भी देखें)
- Clojure Documentation on Strings: [Clojure Strings](https://clojuredocs.org/clojure.string)
- Java String API for context: [Java String API](https://docs.oracle.com/javase/7/docs/api/java/lang/String.html)
- Regular Expressions in Clojure: [Clojure Regex](https://clojuredocs.org/clojure.core/re-find)
