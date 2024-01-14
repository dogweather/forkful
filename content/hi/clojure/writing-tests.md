---
title:    "Clojure: टेस्ट लिखना"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/hi/clojure/writing-tests.md"
---

{{< edit_this_page >}}

महत्व: टेस्ट लिखने में क्यों संलग्न होना आवश्यक है?

अगर आप कोडिंग में ंआनंद लेते हैं, तो आप बिना टेस्ट लिखे हुए कोड का आनंद नहीं ले पाएंगे। टेस्ट लिखने से, आपको अपने कोड की सत्यता की निश्चितता होती है और आप अपने कोड को अधिक सुधार सकते हैं।

कैसे करें:

आप Clojure में कैसे टेस्ट लिख सकते हैं, इसके बारे में समोचित जानकारी मिलेगी। Clojure में टेस्ट केस लिखने के लिए, आप (ns) नामक फंक्शन का इस्तेमाल कर सकते हैं। नीचे दिये गये उदाहरण में, हम एक छोटा सा स्ट्रिंग "Hello" टेस्ट करेंगें।

```Clojure
(ns my-namespace.test
  (:require [clojure.test :refer :all]))

(deftest my-test
    (testing "Hello test"
        (is (= "Hello" "Hello"))))
```

उत्पादन:

```
Testing my-test
FAIL in (my-test) (form-init72.clj:5)
Hello test
expected: (= "Hello" "Hello")
actual: (not (= "Hello" "Hello"))
```

डिप डाइव:

टेस्ट केस को समझने के लिए, आपको माइंडसेट बनाना होगा कि आप अपने कोड को कैसे टेस्ट करना चाहते हैं और कैसे अपने कोड को खराब स्थिति से बचाएं। हर टेस्ट केस में, आप इन प्रश्नों के जवाब ढूंढ सकते हैं।

See Also:

- [Clojure Testing Library](https://github.com/clojure/tools.deps.alpha)
- [Clojure Test Examples](https://clojure.org/guides/devtesting)
- [Clojure Testing Tutorial](https://www.braveclojure.com/testing/)