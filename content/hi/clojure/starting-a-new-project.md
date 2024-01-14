---
title:    "Clojure: एक नई प्रोजेक्ट शुरू करना"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

## क्यों

नए प्रोजेक्ट का आरंभ क्यों करना? पहले से ही कई दूसरे प्रोजेक्ट हो सकते हैं, वह भी किसी अन्य प्रोग्रामिंग भाषा में। फिर भी, क्योंकि क्लोजूर जैसी भाषा दुनियाभर मेंं चर्चित है, आपको क्लोजूर में प्रोजेक्ट शुरू करना चाहिए। यह एक विशेष भाषा है जो व्यावसायिक अनुप्रयोगों और विषय क्षेत्रों में इस्तेमाल की जाती है।

## कैसे से

क्लोजूर प्रोजेक्ट शुरू करने के लिए, आपको पहले अपने सिस्टम में इसे स्थापित करना होगा। फिर आप क्लोजूर कोड लिख सकते हैं जिस्में निम्नलिखित उदाहरण और स्निपेट्स शामिल हो सकते हैं।

```Clojure
;; लिस्ट बनाएं
(def fruits ["आंवला" "मौसमी" "केला"])

;; फाइल बनाएं और पाठ में सहेजें
(spit "fruits.txt" (clojure.string/join "\n" fruits))

;; फ़ंक्शन बनाएं और उसका उपयोग करें
(defn double [x] (* x 2))
(double 5) ;=> 10

;; कंडीशनल लोजिक के साथ फ़ंक्शन बनाएं
(defn even? [x]
  (if (even? x)
    "समान" "असमान"))
(even? 8) ;=> "समान"
(even? 7) ;=> "असमान"

;; वेब पृष्ठ पर सामग्री प्राप्त करने का उदाहरण
(require '[clojure.data.json :as json])
(require '[clj-http.client :as client])

(def url "https://jsonplaceholder.typicode.com/todos/1")
(def response (client/get url))
(def todo (json/read-str (:body response)))

;; डेटा प्रिंट करें
(pretty println todo)
```

```
{
  "userId": 1,
  "id": 1,
  "title": "वान थिंग",
  "completed": false
}
```

## गहराई में

क्लोजूर में प्रोजेक्ट शुरू करने के लिए सबसे पहले आपको इस भाषा को समझना होगा। यह भाषा लिस्ट, मैप, एटम