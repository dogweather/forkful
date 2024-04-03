---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:30:57.293657-07:00
description: "\u0915\u0948\u0938\u0947: Clojure, JVM \u0915\u093E \u0909\u092A\u092F\
  \u094B\u0917 \u0915\u0930\u0924\u0947 \u0939\u0941\u090F, \u0935\u093F\u092D\u093F\
  \u0928\u094D\u0928 \u092A\u0930\u0940\u0915\u094D\u0937\u0923 \u092B\u094D\u0930\
  \u0947\u092E\u0935\u0930\u094D\u0915 \u0915\u093E \u0938\u092E\u0930\u094D\u0925\
  \u0928 \u0915\u0930\u0924\u093E \u0939\u0948\u0964 \u0939\u093E\u0932\u093E\u0902\
  \u0915\u093F, \u0906\u092E\u0924\u094C\u0930 \u092A\u0930 \u0909\u092A\u092F\u094B\
  \u0917 \u0915\u093F\u092F\u093E \u091C\u093E\u0928\u0947 \u0935\u093E\u0932\u093E\
  \ \u090F\u0915 \u092C\u093F\u0932\u094D\u091F-\u0907\u0928 \u0932\u093E\u0907\u092C\
  \u094D\u0930\u0947\u0930\u0940\u2026"
lastmod: '2024-03-13T22:44:51.669435-06:00'
model: gpt-4-0125-preview
summary: "Clojure, JVM \u0915\u093E \u0909\u092A\u092F\u094B\u0917 \u0915\u0930\u0924\
  \u0947 \u0939\u0941\u090F, \u0935\u093F\u092D\u093F\u0928\u094D\u0928 \u092A\u0930\
  \u0940\u0915\u094D\u0937\u0923 \u092B\u094D\u0930\u0947\u092E\u0935\u0930\u094D\u0915\
  \ \u0915\u093E \u0938\u092E\u0930\u094D\u0925\u0928 \u0915\u0930\u0924\u093E \u0939\
  \u0948\u0964 \u0939\u093E\u0932\u093E\u0902\u0915\u093F, \u0906\u092E\u0924\u094C\
  \u0930 \u092A\u0930 \u0909\u092A\u092F\u094B\u0917 \u0915\u093F\u092F\u093E \u091C\
  \u093E\u0928\u0947 \u0935\u093E\u0932\u093E \u090F\u0915 \u092C\u093F\u0932\u094D\
  \u091F-\u0907\u0928 \u0932\u093E\u0907\u092C\u094D\u0930\u0947\u0930\u0940 `clojure.test`\
  \ \u0939\u0948\u0964 \u092F\u0939\u093E\u0901 \u090F\u0915 \u0938\u093E\u0927\u093E\
  \u0930\u0923 \u0909\u0926\u093E\u0939\u0930\u0923 \u0939\u0948."
title: "\u091F\u0947\u0938\u094D\u091F \u0932\u093F\u0916\u0928\u093E"
weight: 36
---

## कैसे:
Clojure, JVM का उपयोग करते हुए, विभिन्न परीक्षण फ्रेमवर्क का समर्थन करता है। हालांकि, आमतौर पर उपयोग किया जाने वाला एक बिल्ट-इन लाइब्रेरी `clojure.test` है। यहाँ एक साधारण उदाहरण है:

```clojure
(ns example.test
  (:require [clojure.test :refer :all]
            [example.core :refer :all]))

(deftest test-addition
  (testing "अंकगणित का परिक्षण"
    (is (= 4 (add 2 2)))
    (is (= 7 (add 3 4)))))

(run-tests)
```
इस परीक्षण को चलाने के बाद, आपको इसी तरह का आउटपुट दिखेगा:

```
Testing example.test

Ran 2 tests containing 2 assertions.
0 failures, 0 errors.
```

जो लोग अधिक सुविधा-समृद्ध विकल्प चाहते हैं, वे `Midje` या `test.check` जैसे तीसरे पक्ष की लाइब्रेरीज का उपयोग कर सकते हैं। ऐसा परीक्षण Midje के साथ कैसे करें, यह यहाँ दिया गया है:

पहले, अपनी project.clj निर्भरताओं में Midje जोड़ें:
```clojure
[midje "1.9.9"]
```

तब, Midje के साथ आपका परीक्षण इस प्रकार दिखेगा:

```clojure
(ns example.test
  (:require [midje.sweet :refer :all]
            [example.core :refer :all]))

(fact "अंकगणित का परिक्षण"
  (add 2 2) => 4
  (add 3 4) => 7)
```

`lein midje` के साथ परीक्षण चलाने पर, आउटपुट कुछ इस प्रकार दिखाई देगा:

```
All checks (2) succeeded.
```
