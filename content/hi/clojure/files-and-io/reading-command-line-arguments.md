---
date: 2024-01-20 17:55:53.276370-07:00
description: "How to: Clojure \u092E\u0947\u0902, \u0906\u092A `*command-line-args*`\
  \ \u0915\u093E \u0907\u0938\u094D\u0924\u0947\u092E\u093E\u0932 \u0915\u0930\u0915\
  \u0947 \u0915\u092E\u093E\u0902\u0921 \u0932\u093E\u0907\u0928 \u0938\u0947 \u0906\
  \u0930\u094D\u0917\u0941\u092E\u0947\u0902\u091F\u094D\u0938 \u092A\u095D \u0938\
  \u0915\u0924\u0947 \u0939\u0948\u0902\u0964."
lastmod: '2024-03-13T22:44:51.690039-06:00'
model: gpt-4-1106-preview
summary: "Clojure \u092E\u0947\u0902, \u0906\u092A `*command-line-args*` \u0915\u093E\
  \ \u0907\u0938\u094D\u0924\u0947\u092E\u093E\u0932 \u0915\u0930\u0915\u0947 \u0915\
  \u092E\u093E\u0902\u0921 \u0932\u093E\u0907\u0928 \u0938\u0947 \u0906\u0930\u094D\
  \u0917\u0941\u092E\u0947\u0902\u091F\u094D\u0938 \u092A\u095D \u0938\u0915\u0924\
  \u0947 \u0939\u0948\u0902\u0964."
title: "\u0915\u092E\u093E\u0902\u0921 \u0932\u093E\u0907\u0928 \u0906\u0930\u094D\
  \u0917\u0941\u092E\u0947\u0902\u091F\u094D\u0938 \u092A\u0922\u093C\u0928\u093E"
weight: 23
---

## How to:
Clojure में, आप `*command-line-args*` का इस्तेमाल करके कमांड लाइन से आर्गुमेंट्स पढ़ सकते हैं।

```Clojure
; args.clj के नाम से फाइल बनाएं और ये कोड उसमें लिखें

(defn -main
  [& args]
  (println "Command line arguments: " args))

; टर्मिनल में lein run चला कर देखें
```

अगर आप इसे चलाते हैं `lein run arg1 arg2 arg3`, आपको आउटपुट मिलेगा:

```
Command line arguments:  (arg1 arg2 arg3)
```

## Deep Dive
Clojure में कमांड लाइन आर्गुमेंट्स को पढ़ने की सुविधा JVM (Java Virtual Machine) के जरिये आती है, क्योंकि Clojure जावा पर आधारित है। स्क्रिप्ट या एप्लिकेशन की फ्लेक्सिबिलिटी के लिए ये तरीका बहुत पुराना और सिद्ध है। `*command-line-args*` एक बिल्ट-इन वैरिएबल है जो एक लाजी सिक्वेंस में आर्गुमेंट्स को स्टोर करता है। इसे दूसरे फंक्शन्स में पास करके आप अपने कोड को अधिक मॉड्यूलर बना सकते हैं।

Clojure की जगह आप सीधे जावा के `main` फंक्शन में `String[] args` का इस्तेमाल कर सकते हैं, या शेल स्क्रिप्टिंग जैसे alternatives भी चुन सकते हैं। पर Clojure का सिंटैक्स और फंक्शनल एप्रोच इसे और आसान बना देते हैं।

## See Also
- [Clojure Docs: command-line-args](https://clojuredocs.org/clojure.core/*command-line-args*)
- [Clojure for the Brave and True - Basic Emacs](https://www.braveclojure.com/basic-emacs/)
