---
title:                "Clojure: स्ट्रिंग की लंबाई का पता लगाना"
simple_title:         "स्ट्रिंग की लंबाई का पता लगाना"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/clojure/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## क्यों

किसी व्यक्ति को स्ट्रिंग की लंबाई निकालने में खुशी मिलती है, यह यह जानने के लिए कि एक स्ट्रिंग में कितने अक्षर हैं।

## कैसे करें

जब हम इंगित कटाव को स्पंश करते हैं, तो यह `count` फ़ंक्शन को कॉल करता है जो उस स्ट्रिंग का आकार (`size`) रिक्त (`empty`) यंत्र की अद्भुत स्वतंत्रता देता है।

```Clojure
(defn find-length [string]
  (count string))

(find-length "नमस्ते") ; Output: 6
```

## गहराई से जाएँ

स्ट्रिंग की लंबाई निकालने के तरीके कई हो सकते हैं, लेकिन `count` फ़ंक्शन एक आसान और अधिकांश मामलों में प्रभावी तरीका है। यह एक बहुत ही साधारण पहला कदम है जो क्रम में इंगित कटाव को अधिक स्पष्ट बनाएगा।

## देखें

[क्लोजर डेटा टाइप](https://clojure.org/reference/data_structures)  
[Clojure की मूलभूत सिंटेक्स](https://clojure.org/guides/getting_started)  
[स्ट्रिंग हैंडलिंग टूल्स की एक सूची](https://github.com/athityakumar/formatter.nvim#string-handling)