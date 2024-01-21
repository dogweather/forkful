---
title:                "यादृच्छिक संख्याएँ उत्पन्न करना"
date:                  2024-01-20T17:49:07.354599-07:00
model:                 gpt-4-1106-preview
simple_title:         "यादृच्छिक संख्याएँ उत्पन्न करना"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/clojure/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)

यादृच्छिक संख्या उत्पन्न करना मतलब है ऐसी संख्याएं बनाना जो पूर्वानुमान योग्य न हो। प्रोग्रामर्स इसे खेलों, सुरक्षा, सिमुलेशन, और डेटा विश्लेषण में इस्तेमाल करते हैं।

## How to: (कैसे करें:)

Clojure में random नंबर्स उत्पन्न करना बिल्कुल सरल है। नीचे कोड और उदाहरण दिए गए हैं:

```Clojure
; एक यादृच्छिक डबल उत्पन्न करें (0 से 1 के बीच)
(rand)

; 0-99 तक की यादृच्छिक पूर्णांक संख्या उत्पन्न करें
(rand-int 100)

; 1 से 6 के बीच की यादृच्छिक संख्याओं का उत्पादन के लिए एक डाइस रोलर बनाएं
(defn roll-dice []
  (+ 1 (rand-int 6)))

; उदाहरण का परिणाम
(roll-dice)
; Output: यहां कोई भी संख्या 1 से 6 के बीच होगी
```

## Deep Dive (गहन जानकारी):

यादृच्छिक संख्या उत्पन्न करने का इतिहास प्राचीन समय से ही शुरू हो गया था जब लोग फालतू अनुमान लगाने के लिए ज्योतिष और फलकिक घटनाओं का इस्तेमाल करते थे। कंप्यूटर युग में, यादृच्छिकता को एल्गोरिथम्स की मदद से पहुंचाया जाता है, जो वास्तविक यादृच्छिकता का अनुकरण करते हैं और इन्हें पसंदूसराहिक यादृच्छिक संख्या जेनरेटर्स कहते हैं। Clojure का `(rand)` और `(rand-int)` इसी श्रेणी में आते हैं। अगर आपको और अधिक नियंत्रण और यादृच्छिकता की ज़रूरत हो, तो java.util.Random या java.security.SecureRandom को Clojure से इंटरऑपरेट करके इस्तेमाल कर सकते हैं।

## See Also (यह भी देखें):

- Clojure की आधिकारिक डॉक्यूमेंटेशन पर `(rand)` और `(rand-int)` की विस्तार से जानकारी: [https://clojuredocs.org/clojure.core/rand](https://clojuredocs.org/clojure.core/rand)
- `java.util.Random` की जावा डॉक्यूमेंटेशन: [https://docs.oracle.com/javase/8/docs/api/java/util/Random.html](https://docs.oracle.com/javase/8/docs/api/java/util/Random.html)
- SecureRandom के बारे में अधिक गहन अध्ययन: [https://docs.oracle.com/javase/8/docs/api/java/security/SecureRandom.html](https://docs.oracle.com/javase/8/docs/api/java/security/SecureRandom.html)