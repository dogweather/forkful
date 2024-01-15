---
title:                "कम्प्यूटर प्रोग्रामिंग में कमांड लाइन आर्ग्यूमेंट पढ़ना"
html_title:           "Clojure: कम्प्यूटर प्रोग्रामिंग में कमांड लाइन आर्ग्यूमेंट पढ़ना"
simple_title:         "कम्प्यूटर प्रोग्रामिंग में कमांड लाइन आर्ग्यूमेंट पढ़ना"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/clojure/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## क्यों

आपने कभी कभी भंटक-मफत के रास्ते से कई प्रोग्राम देखे होंगे जो आपके सिस्टम या सर्वर को निर्दिष्ट कार्यों को करने के लिए आदेश दिए जाते हैं। ध्यान देने वाली बात है कि, जब हम आपको बताते हैं कि आप कंमांड लाइन आर्गुमेंट्स कैसे पढ़ सकते हैं, तो यह आपको प्रोग्रामिंग में अभी भी ऊंचाइयों तक पहुंचाने में मदद कर सकता है।

## कैसे

जब हमारा प्रोग्राम चलता है तो वे आम तौर पर उसे विशेष विधि देशांतरण चिन्हों से प्रअविन बनाते हैं। जैसे कि ```java -jar myprog.jar``` या ```python -m mymodule```

इन आर्गुमेंट्स को प्रोग्राम में पढ़ने के लिए, आप (अगर डेटाबेस से आर्गुमेंट्स निकालने की कोशिश कर रहे हो) शायद [org.clojure/java.data](https://clojars.org/org.clojure/java.data) और [org.clojure/tools.cli](https://clojars.org/org.clojure/tools.cli) जैसे प्रोजेक्ट्स से मजबूत पहुंच करने में सक्षम होंगे।

अपने कोड में यह चरांश समावेश करें:

```Clojure
(ns myprog
  (:require [clojure.tools.cli :refer [parse-opts]])
  
(def cli-options
  [["-p" "--port PORT" "Port number for server"]
   ["-d" "--database URL" "Database URL to connect to"]])

(def cli-args
  (parse-opts *command-line-args* cli-options))

(if (contains? cli-args :opts)
  (let [port-number (cli-args :opts :port)
        db-url (cli-args :opts :database)]
    ;;; केर आपका कोड यहां वर्क करेगा 
    )) 
```

यहां हम देख सकते हैं कि हमने कुछ आर्गुमेंट्स का स्वीकृती शाखा ````:opts``` में होना होगा, जिसके अंदर हमारे आर्गुमेंट्स को hashmap की तरह इस्तेमाल के लिए उपलब्ध होंगे। हम अपने संग्रहीत आर्गुम