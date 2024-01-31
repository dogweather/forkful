---
title:                "रेगुलर एक्सप्रेशन का उपयोग"
date:                  2024-01-19
html_title:           "Bash: रेगुलर एक्सप्रेशन का उपयोग"
simple_title:         "रेगुलर एक्सप्रेशन का उपयोग"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/clojure/using-regular-expressions.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
रेगुलर एक्सप्रेशंस पैटर्न्स का उपयोग है जो टेक्स्ट मैचिंग में मदद करता है। प्रोग्रामर्स इसका उपयोग डेटा पार्सिंग, वैलिडेशन, और सर्चिंग के लिए करते हैं।

## How to: (कैसे करें:)
Clojure में रेगुलर एक्सप्रेशंस का उपयोग कुछ इस तरह से किया जाता है:

```Clojure
;;स्ट्रिंग में ईमेल ढूँढना
(re-find #"[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,}" "मेरा ईमेल example@example.com है.")
;; आउटपुट: "example@example.com"

;; ईमेल के पैटर्न्स की लिस्ट बनाना 
(re-seq #"[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,}" "भेजें: example1@example.com, example2@example.com")
;; आउटपुट: ("example1@example.com" "example2@example.com")
```

यहाँ `re-find` और `re-seq` फंक्शंस का उपयोग हुआ है जो रेगुलर एक्सप्रेशंस का मैच खोजते हैं।

## Deep Dive (गहराई में जानकारी)
रेगुलर एक्सप्रेशन्स का इतिहास 1950 के दशक से शुरू होता है। समय के साथ, वे विकसित होते गए और आज हर प्रोग्रामिंग भाषा का हिस्सा हैं। Clojure में रेगुलर एक्सप्रेशंस Java की `Pattern` क्लास का उपयोग करते हैं, क्योंकि Clojure JVM के ऊपर बनी है। लेकिन इसके अलावा, पार्सर कॉम्बिनेटर जैसे, `Instaparse` Clojure में उपलब्ध एक विकल्प हैं।

## See Also (और भी देखें)
- Clojure Official Documentation: https://clojure.org/
- Java Pattern class: https://docs.oracle.com/javase/8/docs/api/java/util/regex/Pattern.html
- Instaparse library: https://github.com/Engelberg/instaparse

रेगुलर एक्सप्रेशंस पर Clojure के चित्रण के लिए ऊपर दिए गए लिंक उपयोगी होंगे।
