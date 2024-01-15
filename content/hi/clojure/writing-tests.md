---
title:                "प्रोग्रामिंग की भाषा: टेस्ट लिखना"
html_title:           "Clojure: प्रोग्रामिंग की भाषा: टेस्ट लिखना"
simple_title:         "प्रोग्रामिंग की भाषा: टेस्ट लिखना"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/clojure/writing-tests.md"
---

{{< edit_this_page >}}

## क्यों

टेस्ट लिखना क्यों महत्वपूर्ण है? यह सही मानवीय त्रुटियों को पकड़ने और अपने कोड की सुरक्षा को सुनिश्चित करने का एक महत्वपूर्ण तरीका है। यह कोड की गुणवत्ता को बढ़ाता है और ग्राहकों को एक उत्तम उपयोगरत सामग्री प्रदान करता है।

## कैसे करें

```
Clojure
(defn square [n] (* n n)) 

(square 5) 
```
```
25
```

टेस्ट केस लिखने के लिए, हम `deftest` शब्द का इस्तेमाल कर सकते हैं। यह हमें केवल एक ही तरीके के दौरान यह सुनिश्चित करने के लिए हमारे कोड काम कर रहा है। आप भी अलग-अलग तरीकों से अपने फंक्शन्स के लिए टेस्ट केस लिख सकते हैं।

```
Clojure
(deftest test-square
  (is (= 25 (square 5))))

```

## गहराई में जाएँ

टेस्ट लिखना एक आसान काम नहीं है, लेकिन यह आपके कोड को सुरक्षित और स्थिर बनाने में मदद कर सकता है। एक अच्छे टेस्ट स्यूट को लिखने के लिए निम्नलिखित तरीके का पालन करें:

- अपने सभी फंक्शन्स के लिए टेस्ट केस लिखना
- विभिन्न कॉर्नर केस के साथ अपने फंक्शन्स को टेस्ट करना
- अपने कोड का पीछा करना और सुनिश्चित करना कि कोई त्रुटि है नहीं

## देखें भी

- [Clojure Testing for the Brave and True](https://practicalli.github.io/clojure/testing/testing.html)
- [A Comprehensive Guide to Testing in Clojure](https://blog.juxt.pro/posts/comprehensive-guide-testing.html)
- [Clojurists Together Guide to Testing in Clojure](https://www.clojuriststogether.org/news/testing-guide/)