---
title:                "Clojure: टेस्टों की लेखन"
programming_language: "Clojure"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/clojure/writing-tests.md"
---

{{< edit_this_page >}}

## क्यों
टेस्ट लिखने के फायदे हैं क्योंकि वे प्रोग्राम को सुरक्षित बनाकर और बग्स को खोजने में मदद करते हैं। यह प्रोग्राम को दूरस्थ परीक्षण के लिए भी अधिक सुविधाजनक बनाता है।

## कैसे करें
```Clojure
(ns example.core-test
       (:require [clojure.test :refer :all]
                  [example.core :refer :all]))

(deftest addition-test
  (testing "दो संख्याओं को जोड़ना"
    (is (= (addition 2 4) 6)))
  (testing "सही विकल्प के साथ गलत नंबरों की जोड़"
    (is (= (addition "two" 4) :error))))

(deftest subtraction-test
  (testing "दो संख्याओं को घटाना"
    (is (= (subtraction 10 5) 5)))
  (testing "सही विकल्प के साथ गलत नंबरों की घटाई"
    (is (= (subtraction "ten" 5) :error))))
```

## गहराई में जाएँ
टेस्ट लिखने में गहराई में जाने के लिए, हमें अपने कोड के सभी पीसीजी और यूनिट टेस्ट को हल करना होगा। इसमें प्रोग्राम को टेस्ट करने के लिए क्लोजर यूनिट टेस्टिंग में। यह ब्लोग पोस्ट प्रोग्रामिंग और क्लोजर अधिक जानकारी और संदर्भ के लिए उपयोगी हो सकता है।

## देखें भी
- [क्लोजर यूनिट टेस्टिंग](https://www.tutorialspoint.com/clojure/clojure_testing.htm)
- [एफपीआई के लिए क्लोजर अभिलेख](https://clojure.github.io/clojure/clojure.test-api.html)