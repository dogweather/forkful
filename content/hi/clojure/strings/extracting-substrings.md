---
date: 2024-01-20 17:45:21.873201-07:00
description: "How to: (\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902:) Clojure\
  \ \u092E\u0947\u0902 substrings \u0928\u093F\u0915\u093E\u0932\u0928\u0947 \u0915\
  \u0947 \u0932\u093F\u090F `subs` \u092B\u0902\u0915\u094D\u0936\u0928 \u0915\u093E\
  \ \u092A\u094D\u0930\u092F\u094B\u0917 \u0939\u094B\u0924\u093E \u0939\u0948\u0964\
  \ \u0928\u0940\u091A\u0947 \u0915\u0941\u091B \u0909\u0926\u093E\u0939\u0930\u0923\
  \ \u0926\u093F\u090F \u0917\u090F \u0939\u0948\u0902."
lastmod: '2024-03-13T22:44:51.644163-06:00'
model: gpt-4-1106-preview
summary: "Clojure \u092E\u0947\u0902 substrings \u0928\u093F\u0915\u093E\u0932\u0928\
  \u0947 \u0915\u0947 \u0932\u093F\u090F `subs` \u092B\u0902\u0915\u094D\u0936\u0928\
  \ \u0915\u093E \u092A\u094D\u0930\u092F\u094B\u0917 \u0939\u094B\u0924\u093E \u0939\
  \u0948\u0964 \u0928\u0940\u091A\u0947 \u0915\u0941\u091B \u0909\u0926\u093E\u0939\
  \u0930\u0923 \u0926\u093F\u090F \u0917\u090F \u0939\u0948\u0902."
title: "\u0938\u092C\u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917\u094D\u0938 \u0928\
  \u093F\u0915\u093E\u0932\u0928\u093E"
weight: 6
---

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
