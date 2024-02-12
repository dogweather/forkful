---
title:                "कमांड लाइन आर्गुमेंट्स पढ़ना"
aliases:
- hi/clojure/reading-command-line-arguments.md
date:                  2024-01-20T17:55:53.276370-07:00
model:                 gpt-4-1106-preview
simple_title:         "कमांड लाइन आर्गुमेंट्स पढ़ना"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/clojure/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## What & Why?
कमांड लाइन आर्गुमेंट्स पढ़ना ऐप्लिकेशन को बाहरी इनपुट देने का एक तरीका है। प्रोग्रामर्स इसे इस्तेमाल करते हैं ताकि यूजर के सीधे इनपुट द्वारा प्रोग्राम की फंक्शनैलिटी को कस्टमाइज कर सकें।

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
