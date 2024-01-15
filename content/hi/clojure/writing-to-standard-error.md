---
title:                "मानक त्रुटि पर लिखना"
html_title:           "Clojure: मानक त्रुटि पर लिखना"
simple_title:         "मानक त्रुटि पर लिखना"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/clojure/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## क्यों

अगर आप Clojure का उपयोग करते हैं, तो आपने शायद विभिन्न त्रुटियों या समस्याओं का सामना किया होगा। ये त्रुटियां आपकी प्रोग्रामिंग को ठीक से काम नहीं करने की वजह से होती हैं। इसलिए, Clojure में `standard error` का उपयोग करके आप अपनी त्रुटियों को ठीक कर सकते हैं। इससे आपको अपनी प्रोग्रामिंग की गुणवत्ता में सुधार मिल सकता है।

## कैसे करें

अगर आपको Clojure में `standard error` का उपयोग करना है, तो आपको जानना होगा कि आपको `clojure.core / eprint` फ़ंक्शन का उपयोग करना होगा। यह फ़ंक्शन एक स्ट्रिंग या किसी अन्य डेटा टाइप को दूसरे पैरामीटर के रूप में ले और उसे `standard error` के माध्यम से प्रिंट करता है। नीचे लिखे कोड ब्लॉक में आप इसका एक उदाहरण देख सकते हैं:

```Clojure
(defn divide [num1 num2]
  (if (= num2 0)
    (eprint "Error: Cannot divide by zero")
    (/ num1 num2)))

(divide 10 5) ; Output: 2

(divide 10 0) ; Output: Error: Cannot divide by zero
```

## गहराई में जाएं

`clojure.core / eprint` फ़ंक्शन का उपयोग करना भले ही आसान हो, लेकिन इसके पीछे होने वाली तकनीकी विवरणों को समझना महत्वपूर्ण है। यह फ़ंक्शन `System/err` वस्तु को उपयोग करता है जो आपको त्रुटियों को प्रिंट करने की अनुमति देती है। इसका उपयोग करते हुए आप अपनी प्रोग्रामिंग में त्रुटियों को फिर से ट्रैक कर सकते हैं और उन्हें ठीक कर सकते हैं।

## फिर भी चिंतित हो?

क्या आपको अ