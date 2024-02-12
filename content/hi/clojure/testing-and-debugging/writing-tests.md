---
title:                "टेस्ट लिखना"
aliases:
- hi/clojure/writing-tests.md
date:                  2024-02-03T19:30:57.293657-07:00
model:                 gpt-4-0125-preview
simple_title:         "टेस्ट लिखना"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/clojure/writing-tests.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## क्या और क्यों?
Clojure में परीक्षण लेखन, अन्य प्रोग्रामिंग भाषाओं की तरह, आपके मुख्य कोडबेस के अपेक्षित रूप से काम करने की पुष्टि करने के लिए समर्पित कोड बनाने को शामिल करता है। यह सहीपन, पुनर्गठन में सहायता, और कोड स्थिरता में वृद्धि में मदद करता है।

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
