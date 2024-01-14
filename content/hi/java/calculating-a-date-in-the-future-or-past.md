---
title:                "Java: भविष्य या भूतकाल में तारीख की गणना"
simple_title:         "भविष्य या भूतकाल में तारीख की गणना"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/java/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## क्यों
जब हम एक टॉल बॉय या कैलेंडर का बिल भुगतान करने के लिए जाते हैं, हमें कभी-कभी भविष्य के लिए तारीख का पता लगाने की जरूरत पड़ सकती है। ऐसे मामलों में, हमारे पास आज की तारीख से कुछ दिन पहले या भविष्य में दिनों की संख्या को जानने की आवश्यकता होती है। इस ब्लॉग पोस्ट में, हम इस प्रक्रिया को कैसे कर सकते हैं और इसके पीछे की विस्तृत जानकारी देखेंगे।

## कैसे करें
``` java
import java.time.LocalDate;

// आज की तारीख को प्राप्त करें
LocalDate today = LocalDate.now();

// 10 दिन बाद की तारीख को प्राप्त करें
LocalDate futureDate = today.plusDays(10);

// 5 दिन पहले की तारीख को प्राप्त करें
LocalDate pastDate = today.minusDays(5);

// हमें संख्या नहीं, बल्कि तारीख चाहिए है, तो कैसे करें?
// हम अपने Output को आवश्यक फॉर्मेट में बदल सकते हैं 
// उदाहरण के लिए: "28 मार्च, 2020"
String formattedFutureDate = futureDate.format(DateTimeFormatter.ofPattern("dd MMMM, yyyy"));
```
यहां हम `java.time.LocalDate` कक्षा का उपयोग करके Java में तारीखों को प्रस्तुत करते हैं। हम मौजूदा तारीख से दिनों को जोड़ने और घटाने के लिए `plusDays()` और `minusDays()` मेथड का उपयोग कर सकते हैं। इसके अलावा, हम `DateTimeFormatter` का उपयोग करके तारीख को आवश्यक फॉर्मेट में प्रदर्शित कर सकते हैं।

## गहराई में जाएं
इस प्रक्रिया के आसान उदाहरणों के अतिरिक्त, देखें कि हमले Java में तारीखों को कैसे हंगामा महकाते हैं। हम मौजूदा तारी