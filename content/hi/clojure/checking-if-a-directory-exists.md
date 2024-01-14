---
title:    "Clojure: एक निर्देशिका मौजूद है कि नहीं जांचना"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/hi/clojure/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## इसका कारण

अगर आप Clojure भाषा में पाठक हैं तो आप निश्चित रूप से जानते हैं कि डाइरेक्ट्री की उपस्थिति की जांच क्यों महत्वपूर्ण है। इसे जांचना आपको इस्तेमाल करने वाले किसी भी फ़ंक्शन के लिए स्नेहित स्रोत से जानने की अनुमति देता है।

## कैसे करें

आप Clojure में डाइरेक्ट्री की उपस्थिति की जांच करने के लिए निम्नलिखित कोड का इस्तेमाल कर सकते हैं:

```Clojure
;; सबसे पहले आप आवश्यक जावास्क्रिप्ट लाइब्रेरी को लोड करें
(.add-to-classpath (java.io.File. "lib/js.jar"))

;; अब आप clojure.java.io नेमस्पेस को लोड कर सकते हैं
(use 'clojure.java.io)

;; आप किसी भी फ़ोल्डर के नाम से नए फ़ाइल ऑब्जेक्ट को बना सकते हैं
(def folder (file "foldername"))

;; अब exists? फ़ंक्शन का इस्तेमाल करें जो डाइरेक्ट्री की उपस्थिति को चेक करेगा
(exists? folder)
```

आपको निम्नलिखित आउटपुट मिलना चाहिए:

```Clojure
true
```

## गहराई में जाएं

इस प्रक्रिया में और विस्तृत ढांचे का अध्ययन करने के लिए, आप वेबसाइट [https://clojuredocs.org/](https://clojuredocs.org/) पर जा सकते हैं। यहां आप Clojure भाषा में बनाए गए उपयोगी फंक्शन के उदाहरण और डॉक्यूमेंटेशन पा सकते हैं। आप अपनी पसंदीदा फंक्शन की गहराई तक जान सकते हैं और उसे अपने काम में उपयोग कर सकते हैं।

## और भी देखें

[ClojureScript में फोल्डर की उपस्थिति की जांच करें](https://himanshsahni.com/clojurescript-in-hindi-directory-exists/)