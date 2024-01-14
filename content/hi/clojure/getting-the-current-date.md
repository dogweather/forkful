---
title:                "Clojure: वर्तमान तारीख प्राप्त करना"
programming_language: "Clojure"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/clojure/getting-the-current-date.md"
---

{{< edit_this_page >}}

# क्यों

आपने शायद सोचा होगा कि प्रोग्रामिंग में तारीख मिलाना क्यों महत्वपूर्ण है। आइये इसका कारण जानते हैं।

# कैसे करें

 क्लोजर में वर्तमान तारीख को प्राप्त करना बहुत आसान है। निम्नांकित कोड से इसे समझते हैं:

```Clojure
(def today (java.util.Date.))
(println today)
```
 उपरोक्त कोड को चलाने पर आपको अपने कंसोल पर वर्तमान तारीख का संबंधित क्षेत्र, समय और वर्ग का नाम मिलेगा।

# गहराई में

जब हम `java.util.Date` क्लास को इन्स्टेंटिएट करते हैं, तो यह हमें वर्तमान समय को मिलाने का सुझाव देता है, जो कि उस समय होता है जब हम इसे इन्स्टेंटिएट करते हैं। अगर आप स्थानीय समय को प्राप्त करना चाहते हैं, तो आप `java.time` पैकेज या उसके `LocalDate` क्लास का उपयोग कर सकते हैं। इस पैकेज में अनेक तरीके शामिल हैं जो आपको अन्य समय क्षेत्र के साथ संबंधित तारीखों को प्राप्त करने में मदद कर सकते हैं।

# देखिए भी

[क्लोजर डॉक्यूमेंटेशन](https://clojuredocs.org/clojure.java-time)
[इस बारे में और जानें](https://www.baeldung.com/java-date-to-localdate-and-localdatetime)
[क्लोजर ट्यूटोरियल](https://www.tutorialspoint.com/clojure/clojure_date_time.htm)