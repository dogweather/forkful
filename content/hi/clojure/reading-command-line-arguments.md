---
title:                "कमांड लाइन तर्कों को पढ़ना"
html_title:           "Kotlin: कमांड लाइन तर्कों को पढ़ना"
simple_title:         "कमांड लाइन तर्कों को पढ़ना"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/clojure/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

आप क्या जाना है: *कमांड लाइन आर्ग्यूमेंट्स क्या होते हैं और उन्हें पढ़ने का क्या इरादा है*। प्रोग्रामर कमांड लाइन आर्ग्यूमेंट्स पढ़ते हैं क्योंकि ये उन्हें उनके कोड में डाइनेमिक परिवर्तन करने का संज्ञान देते हैं।

## कैसे:

Clojure में कमांड लाइन आर्ग्यूमेंट्स को पढ़ने का तरीका ```*command-line-args*``` का इस्तेमाल करना है:

``` Clojure 
(defn -main 
  [& args] 
  (println "Command line arguments are:" args))
```

यानि यदि आप इसे चलाते हैं ```lein run arg1 arg2 arg3``` तो आपका आउटपुट होगा:

```Clojure
Command line arguments are: (arg1 arg2 arg3)
```

## गहरी डाइव

कमांड लाइन आर्ग्यूमेंट्स का इतिहास काफी पुराना है और ये प्रोग्रामिंग की शायरी में काम का आधार हैं। इन्हें पढ़ने के लिए Clojure में अन्य विकल्प भी हैं। ```tools.cli``` एक ऐसा लाइब्रेरी है जिससे आप एडवांस्ड पार्सिंग, वैलिडेशन, और एरर हैंडलिंग जैसी सुविधाओं का उपयोग कर सकते हैं।

## अधिक जानकारी के लिए

अधिक जानकारी के लिए निम्नलिखित संसाधनों का उपयोग करें:

- **Clojure डॉक्स** (http://clojure.org): Clojure का आधिकारिक डॉक्युमेंटेशन, जहां पर आप ```*command-line-args*``` के बारे में विस्तार में जान सकते हैं।
- **Clojure की गूगल ग्रुप** (https://groups.google.com/forum/#!forum/clojure): यहां पर आप अगर कोई समस्या आती है तो उसके लिए समाधान खोज सकते हैं।