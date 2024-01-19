---
title:                "नया प्रोजेक्ट शुरू करना"
html_title:           "C: नया प्रोजेक्ट शुरू करना"
simple_title:         "नया प्रोजेक्ट शुरू करना"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/clojure/starting-a-new-project.md"
---

{{< edit_this_page >}}

## क्या और क्यों? (What & Why?)
नयी प्रोजेक्ट शुरू करना मतलब एक नई सॉफ्टवेयर की बुनियाद रखना । प्रोग्रामर्स इसे करते हैं ताकि वे कीचड़ से बच सकें और अपने प्रोजेक्ट को धीमे करने से रोक सकें।

## कैसे करें: (How to:)

यहाँ Clojure बंद में कैसे शुरुआत करें -
```Clojure 
; सबसे पहले लेटिंगंन को स्थापित करें 
; Clojure प्रोजेक्ट को बनाने और चलाने के लिए 
$ brew install leiningen

; एक नया प्रोजेक्ट बनाएं 
$ lein new my_first_project
```
इसके बाद 
```Clojure 
$ cd my_first_project
$ lein run 
```
## गहरी जानकारी (Deep Dive)

Clojure, जीन्स्टील्स ने 1953 में स्थापित की थी, लिस्प प्रोग्रामिंग भाषा पर आधारित है और Java Virtual Machine (JVM) के लिए डिजाइन की गई है। Clojure का उपयोग वास्तविक समय के डेटा-ढंग संरेखण (data-oriented programming) और मल्टिथ्रेडिंग (multithreading) हेतु किया जाता है। 

वैकल्पिक रूप से, आप `Maven` या `Gradle` जैसे उपकरणों का उपयोग कर सकते हैं लेकिन Clojure के लिए Leiningen एक बहुत ही मजबूत और शानदार साधन है। 

## अतिरिक्त जानकारी के लिए (See Also) 

Leiningen में प्रोजेक्ट बनाने का विस्तृत वर्णन: [यहाँ](https://leiningen.org/#:~:text=Leiningen%20is%20the%20easiest%20way,builds%20and%20dependency%20management%20out.)

Clojure प्रोग्रामिंग के लिए गाइड: [यहाँ](https://clojure.org/guides/getting_started)

Clojure में माल्टीथ्रेडिंग कैसे काम करता है: [यहाँ](https://clojuredocs.org/clojure.core/future)