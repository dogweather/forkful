---
title:    "Clojure: दो तारीखों का तुलना करना"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/hi/clojure/comparing-two-dates.md"
---

{{< edit_this_page >}}

हिंदी भाषी पाठकों के लिए एक कैजुअल क्लोजर प्रोग्रामिंग ब्लॉग पोस्ट।

## क्यों

दो तारीखों को तुलना करने का काम करने के लिए क्यों लोग व्यस्त होते हैं, इसे जानने के लिए एक आसान तरीका है।

## कैसे

तारीखों को तुलना करने के लिए एक पुरानी तारीख और एक नई तारीख को लेकर काम करते हुए, हम समय अंतर को निकाल सकते हैं। इसके लिए हम `clj-time` पैकेज का उपयोग कर सकते हैं।

```Clojure
(require '[clj-time.core :as time])

(def old-date (time/date-time 2020 12 1))
(def new-date (time/date-time 2021 2 28))

(time/interval old-date new-date)
```

आउटपुट:

> #<IntervalP  0:0:0.0/84:0:0.0>

यहां हम `interval` फ़ंक्शन का उपयोग किया है जो दो तारीखों के बीच समय अंतर को निकलता है। हम सेकंड, मिनट, घंटे, दिन, हफ्ते और सालों में यह अंतर निकाल सकते हैं।

## डीप डाइव

तारीखों को तुलना करने की प्रक्रिया समझने से पहले, हमें दो बहुत ही महत्वपूर्ण आदेशों का पालन करना होगा। पहले, हमें `clj-time` पैकेज का उपयोग करके तारीखों को प्रतिनिधित्व करने के लिए दो नए डेटा टाइप्स बनाने होंगे। दूसरा, हमें तारीखों को`comparing` फ़ंक्शन की मदद से तुलना करने के लिए तैयार करना होगा।

```Clojure
(require '[clj-time.core :as time])

(defrecord Date [year month day])
(defn comparing [d1 d2]
    (let [dt1 (time/date-time (:year d1) (:month d1) (:day d1))
          dt2 (time/date-time (:year d2) (:month d2) (:day d2))]
    (time/interval dt1 dt2)))
```

यहां हमने `defrecord` का उपयोग करके `Date` नामक नए डेटा टाइप क