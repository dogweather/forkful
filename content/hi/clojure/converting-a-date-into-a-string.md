---
title:    "Clojure: तारीख को स्ट्रिंग में रूपांतरण करना"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/hi/clojure/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

##क्यों 

कभी-कभी हमें तारीख को एक स्ट्रिंग में बदलने की जरूरत होती है, जैसे कि अपने ब्लॉग पोस्ट में. इसके लिए हमे Clojure में तारीख को स्ट्रिंग में परिवर्तित करना सीखना जरूरी है.

## कैसे करें

```Clojure 
(def my-date (java.util.Date.))
(clojure.string/replace (str my-date) "-" "/")
```

आपको पहले  java.util.Date ऑब्जेक्ट बनाने की जरूरत होगी, और उसको str फ़ंक्शन से स्ट्रिंग में परिवर्तित करना होगा। अगर आपको तारीख को अलग ढंग से प्रदर्शित करना हो तो आप  clojure.string/replace फ़ंक्शन का उपयोग कर सकते हैं।

उक्त कोड का प्रिंट आउट नीचे दिया गया है:

```Clojure
Sun Nov 14 19:33:42 IST 2021
11/14/21 19:33:42
```

## गहराई में डूबों

तारीख को स्ट्रिंग में परिवर्तित करना बहुत आसान है, लेकिन यह जानने में मज़ा आ सकता है कि हम इसे कैसे कर सकते हैं.

जुलियन दिन कोड एक integer नंबर होता है जो हमारे दिनांक से से जूड़ा होता है। हमे इस नंबर को महीनों, दिनों, घंटों, मिनटों आदि में विभाजित करना होगा ताकि हम उसे स्ट्रिंग में अलग अलग जगहों पर प्रदर्शित कर सकें।

## देखें भी

- [Clojure ट्यूटोरियल](https://clojure.org/guides/getting_started)
- [Java डाक्यूमेंटेशन](https://docs.oracle.com/en/java/javase/16/docs/api/java.base/java/util/Date.html)
- [Clojure स्ट्रिंग फंक्शन्स](https://clojure.org/reference/strings)