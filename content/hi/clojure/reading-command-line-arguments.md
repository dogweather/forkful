---
title:                "Clojure: कम्प्यूटर प्रोग्रामिंग में आदेश सामग्री को पढ़ना"
simple_title:         "कम्प्यूटर प्रोग्रामिंग में आदेश सामग्री को पढ़ना"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/clojure/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# क्यों डालना है
लोग कमांड लाइन आर्ग्यूमेंट्स पढ़ने से क्यों आकर्षित होते हैं केवल 1-2 वाक्यों में व्याख्या करें।

## उपाय
यह मूल दुविधा है कि कैसे कोडिंग के उपयोग से आप डालने की संभावना है। दर्शाता है। तुम क्लोजुर की Attention कैसे सुधरो और क्या परिणाम Koders प्राप्त कर सकते हो।

### सामान्य उदाहरण
```Clojure
(println "Command line arguments:")
(println *command-line-args*)
```
**आउटपुट:**
Command line arguments:
[:arg1 :arg2 :arg3]

### मामला अदर्न
आप क्लोजुर से आर्ग्युमेंट्स पढ़te हो तो प्रोग्राम चलाने से पहले उन्हें प्रोसेस नना होता है। यह उदाहरण का उपयोग करके वर्ग है

```Clojure
(defn process-arguments
  "र्लाइन वर्ग की  आर्गुमेंट्स की संयोजन करतें”
  [args]
  (println "सभी व सभी नॉश्निक शा")
  (println "आपके दिए गए आर्ग्युमेंट YUs :")
  (doseq [arg args]
    (println arg))))
```

**आउटपुट:**
सभी व हर एक  आलबरोज़ ESBI संयोजन
आपके दिए गए सारथपुर यूसीचूंत RWBERS निस्प एलेंचितर्त andhra
g
SEBIEE Dina Nil nil

## गहन अध्ययन
आर्गुमेंट पढ़ा बात हमें अदिहूहों और कोलासल मापदंडों का दिखाने के लिए जान नि देता है। हम यहां पर सूक्ष्म ताकत और छोटे उदहारण का उपयोग करेंगे। 

## देखें भी
- ऑफिशियल क्लोजुर दस्तावेज़
(https://clojure.org/reference/cli)
- आर्गुमेंट्स कस्टमाेडे।
(https://www.youtube.com/watch?v=K96IzFm_DNM)