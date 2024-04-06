---
date: 2024-01-20 17:42:04.814908-07:00
description: "How to: (\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902:) Clojure\
  \ \u092E\u0947\u0902, \u0939\u092E `re-seq`, `re-find`, `clojure.string/replace`\
  \ \u091C\u0948\u0938\u0947 \u092B\u0902\u0915\u094D\u0936\u0902\u0938 \u0915\u093E\
  \ \u0909\u092A\u092F\u094B\u0917 \u0915\u0930 \u092A\u0948\u091F\u0930\u094D\u0928\
  \ \u0938\u0947 \u092E\u093F\u0932\u0924\u0947 \u091A\u0930\u093F\u0924\u094D\u0930\
  \u094B\u0902 \u0915\u094B \u0906\u0938\u093E\u0928\u0940 \u0938\u0947 \u0939\u091F\
  \u093E \u0938\u0915\u0924\u0947\u2026"
lastmod: '2024-04-05T21:53:53.654618-06:00'
model: gpt-4-1106-preview
summary: "(\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902:) Clojure \u092E\u0947\
  \u0902, \u0939\u092E `re-seq`, `re-find`, `clojure.string/replace` \u091C\u0948\u0938\
  \u0947 \u092B\u0902\u0915\u094D\u0936\u0902\u0938 \u0915\u093E \u0909\u092A\u092F\
  \u094B\u0917 \u0915\u0930 \u092A\u0948\u091F\u0930\u094D\u0928 \u0938\u0947 \u092E\
  \u093F\u0932\u0924\u0947 \u091A\u0930\u093F\u0924\u094D\u0930\u094B\u0902 \u0915\
  \u094B \u0906\u0938\u093E\u0928\u0940 \u0938\u0947 \u0939\u091F\u093E \u0938\u0915\
  \u0924\u0947 \u0939\u0948\u0902\u0964."
title: "\u092A\u0948\u091F\u0930\u094D\u0928 \u0938\u0947 \u092E\u0947\u0932 \u0916\
  \u093E\u0924\u0947 \u0905\u0915\u094D\u0937\u0930\u094B\u0902 \u0915\u094B \u0939\
  \u091F\u093E\u0928\u093E"
weight: 5
---

## How to: (कैसे करें:)
Clojure में, हम `re-seq`, `re-find`, `clojure.string/replace` जैसे फंक्शंस का उपयोग कर पैटर्न से मिलते चरित्रों को आसानी से हटा सकते हैं।

```clojure
; संख्याओं को हटाना
(clojure.string/replace "Clojure123" #"\d+" "")
; Output: "Clojure"

; विशेष चरित्रों को हटाना
(clojure.string/replace "Hello, World!" #"[^\w\s]" "")
; Output: "Hello World"

; बड़े / छोटे अक्षरों को हटाना
(clojure.string/replace "Clojure Programming" #"[A-Z]" "")
; Output: "lojure rogramming"
```

## Deep Dive (गहराई में जानकारी):
चरित्र हटाने के लिए RegEx (Regular Expressions) का इस्तेमाल पुराने समय से ही हो रहा है। Clojure भी JVM (Java Virtual Machine) आधारित होने के कारण Java के RegEx इंजन का उपयोग करता है।

1. Historical context: RegEx प्रोसेसिंग अन्य प्रोग्रामिंग भाषाओं में भी मिलती है, और यह टेक्स्ट प्रोसेसिंग के सबसे शक्तिशाली टूल्स में से एक है।
2. Alternatives: कुछ केसेस में `string/split`, `filter`, या `remove` जैसे कोर फंक्शंस से भी पैटर्न हटाना संभव है, लेकिन वे RegEx की तुलना में कम लचीले होते हैं।
3. Implementation details: Clojure में `re-pattern` फंक्शन का प्रयोग कर कस्टम RegEx पैटर्न्स बनाए जा सकते हैं, और `re-matcher`, `re-matches`, `re-groups` जैसे फंक्शंस के साथ काम किया जा सकता है।

## See Also (इसे भी देखें):
- Clojure के आधिकारिक डाक्यूमेंटेशन में [Regular Expressions](https://clojure.org/guides/learn/functions#_regular_expressions) सेक्शन
- [ClojureDocs](https://clojuredocs.org/) पर `clojure.string/replace`
- [Brave Clojure](https://www.braveclojure.com/) पर RegEx पर अधिकः जानकारी
