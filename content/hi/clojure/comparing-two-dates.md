---
title:                "Clojure: कंप्यूटर प्रोग्रामिंग में दो तारीखों की तुलना करना"
programming_language: "Clojure"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/clojure/comparing-two-dates.md"
---

{{< edit_this_page >}}

## क्यों

जब हम दो तारीखों को तुलना करते हैं, तो हम उनमें से एक की प्रोग्रामिंग में उपयोग कर सकते हैं। इससे हमारे कोड को अधिक सुस्त बनाने में मदद मिलती है और हम गलतियों को भी रोक सकते हैं।

## कैसे

```Clojure
(def today (java.util.Date.))
(def yesterday (java.util.Date. (- (.getTime today) (* 24 60 60 1000))))

(time-in-ms today)
=> 1606711413815

(time-in-ms yesterday)
=> 1606625013815

(time-ago today yesterday)
=> #object[java.time.Duration 0x7c5a6700 "PT-21H"]
```

ऊपर दिए गए कोड ब्लॉक में, हमने दो जेवा डेट ऑब्जेक्ट बनाएं और उनके बीच का समय गिनाया। फिर हमने उसी समय के अंतर को दिखाया। "time-in-ms" फ़ंक्शन द्वारा दो तारीखों को मिलाकर अंतर निकाला गया है और "time-ago" फ़ंक्शन द्वारा हमने उसमें से पहली तारीख को कम कर दिया है। जब हम इसे प्रिंट करते हैं, तो हमें उस समय का अंतर प्राप्त होता है।

## डीप डाइव

दो तारीखों को तुलना करने के लिए, हम इनमें से एक की सेमिंग क्रिया करते हैं। यह दोनों तारीखों को मिलाकर एक नई तारीख बनाता है जो हमारे काम के लिए उपयोगी होती है। इसके अलावा, हम प्यारेन्थेसिस द्वारा भी तारीखों को पीढ़ित कर सकते हैं।

## देखें अन्यथा

- [Clojure ऑफिशियल वेबसाइट](https://clojure.org/)
- [Clojure का GitHUb रिपोजिटरी](https://github.com/clojure/clojure)
- [आरंभिक Clojure प्रोग्रामिंग के लिए सीखें](https://www.tutorialspoint.com/clojure/index.htm)