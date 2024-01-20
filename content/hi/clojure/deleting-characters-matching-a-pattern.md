---
title:                "पैटर्न से मिलते जुलते वर्णों को हटाना"
html_title:           "Elixir: पैटर्न से मिलते जुलते वर्णों को हटाना"
simple_title:         "पैटर्न से मिलते जुलते वर्णों को हटाना"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/clojure/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

विचारिए कि आपको किसी विशेष पैटर्न से मेल खाने वाले किरदारों को हटाना हो। यह बहुत ही सामान्य काम होता है - समय को सहेजने के लिए, कोड को स्वच्छ बनाने के लिए, या डेटा को सही तरीके से संसाधित करने के लिए।

## कैसे करें:

Clojure में, पैटर्न से मेल खाने वाले किरदारों को हटाने के लिए हम ```clojure.string/replace``` का उपयोग कर सकते हैं:

```Clojure
(require '[clojure.string :as str])

(defn delete-matching-chars [input pattern]
  (str/replace input pattern ""))
```
सम्पादक में, उपरोक्त कोड कतार को चलाने पर, आपको नीचे वाला आउटपुट मिलेगा:

```Clojure
(delete-matching-chars "Hello, World!" ",")
; "Hello World!"
```

## गहराई में जाने:

Clojure का इस्तेमाल 2007 में प्रारंभ हुआ था, और इसने लिस्प के विचारों को JVM पर पुनर्निर्मित किया। इसके विकल्प के रूप में  'java.util.regex' और `java.lang.String/replaceAll` जैसी API भी हैं।

तथापि, Clojure के `clojure.string/replace` का इस्तेमाल करना अधिक आसान हो जाता है, जबकि यह जावा आपरेशन्स के लिए सीधी एक्सेस प्रदान करता है, इसे उपयोगिता के लिए ओप्टाइमाइज़ किया जा सकता है।

## अधिक जानकारी:

- Clojure की API [clojure.string/replace](https://clojuredocs.org/clojure.string/replace)
- Clojure के लिए [Practical LiSP](http://www.gigamonkeys.com/book/) बुक
- Clojure के बारे में [StackOverflow](https://stackoverflow.com/questions/tagged/clojure) पर सवाल जवाब