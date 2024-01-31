---
title:                "परीक्षण लिखना"
date:                  2024-01-19
simple_title:         "परीक्षण लिखना"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/clojure/writing-tests.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
टेस्ट लिखना यानी अपने कोड का परीक्षण करना, ताकि बग्स को ढूंढा जा सके और फ़्यूचर में कोड में बदलाव करते समय सुरक्षा की गारंटी हो। प्रोग्रामर इसे इसलिए करते हैं ताकि उन्हें विश्वास हो सके कि उनका कोड सही तरीके से काम कर रहा है।

## कैसे करें:
```Clojure
(ns example.test
  (:require [clojure.test :refer :all]
            [example.core :refer :all]))

(deftest a-test-function
  (testing "साधारण जोड़"
    (is (= 4 (add 2 2)))))

(run-tests)

```
ऊपर दिया गया कोड, `example.core` नामक नेमस्पेस में `add` फ़ंक्शन का टेस्ट करता है। अगर टेस्ट पास होता है, तो आउटपुट दिखाएगा:

```
Testing example.test

Ran 1 tests containing 1 assertions.
0 failures, 0 errors.
```

## गहराई से समझ:
टेस्ट लिखने का चलन 1950 के दशक से शुरू हुआ था। जैसे-जैसे कंप्यूटर प्रोग्राम्स पेचीदा हुए, टेस्टिंग की अहमियत भी बढ़ी। Clojure में, `clojure.test` लाइब्रेरी ज्यादा इस्तिमाल की जाती है, पर विकल्प के रूप में Midje और Expectations जैसे टूल्स भी मौजूद हैं। डेटेल्स जैसे कि fixtures और mock ऑब्जेक्ट्स का इस्तेमाल करके टेस्टिंग को और गहरा बनाया जा सकता है।

## देखें भी:
- Clojure की आधिकारिक टेस्टिंग गाइड: https://clojure.org/guides/deps_and_cli#_testing
- `clojure.test` लाइब्रेरी डॉक्स: https://clojuredocs.org/clojure.test
- Midje का GitHub पेज: https://github.com/marick/Midje
- Expectations लाइब्रेरी का GitHub पेज: https://github.com/clojure-expectations/expectations
