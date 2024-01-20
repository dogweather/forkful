---
title:                "नियमित अभिव्यक्तियों का उपयोग करना"
html_title:           "Arduino: नियमित अभिव्यक्तियों का उपयोग करना"
simple_title:         "नियमित अभिव्यक्तियों का उपयोग करना"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/clojure/using-regular-expressions.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

नियमित अभिव्यक्तियों का उपयोग पाठ डेटा के नमूनों (patterns) को मिला और मिलाने के लिए होता है। प्रोग्रामर्स इसे कार्य प्रवाह को बचती समय और सुधारे गए सत्यापन के लिए उपयोग करते हैं।

## कैसे:

```clojure
(defn pattern-match [re string]
  (re-find (re-pattern re) string))

(pattern-match "\\d+" "Hello123")  ; ➞ "123"
```
यह कोड Clojure में एक नियमित अभिव्यक्ति मिलाने के लिए `re-find` और `re-pattern` फंक्शन्स का उपयोग करता है। यह "Hello123" में दिए गए पहली संख्या श्रृंखला को लौटाएगा, जो इस मामले में "123" है।

## गहरी डुबकी
1. इतिहास: नियमित अभिव्यक्तियां केन थॉम्प्सन द्वारा 1960 में Unix टेक्स्ट एडिटर "ed" के लिए विकसित की गई थी।
2. विकल्प: पाठ पैटर्न मिलान के अन्य तरीके में वाईल्डकार्ड, ग्लोबिंग, और धागे समावेश होते हैं।
3. कार्यान्वयन: Clojure में, नियमित अभिव्यक्तियों को JVM के वर्गों के जरिए लागू किया जाता है, देखें `java.util.regex`.

## अधिक देखें

1. ClojureDoc पर नियमित अभिव्यक्तियाँ: https://clojuredocs.org/clojure.core/re-find
2. Clojure प्रोग्राम की उचित बुनियादी: https://www.braveclojure.com/assets/clojure-for-the-brave-and-true.pdf
3. Java नियमित अभिव्यक्ति गाइड: http://tutorials.jenkov.com/java-regex/index.html