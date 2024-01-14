---
title:    "Java: दो तारीखों की तुलना"
keywords: ["Java"]
---

{{< edit_this_page >}}

## क्यों

क्या आप भी कभी दो तारीखों को संबोधित करने का काम किया है? प्रोग्रामिंग में आपको विभिन्न तारीखों को तुलना करने की आवश्यकता पड़ सकती है, जैसे कि उपयोगकर्ता के जन्मदिन को समय के साथ तुलना करना या दो तारीखों के बीच कितने दिन हैं या कितने साल बीते हैं। इसलिए, हम एक जावा में दो तारीखों की तुलना क्रिया को जानने का इच्छुक हैं।

## कैसे करें

जावा में दो तारीखों को संबोधित करने के लिए, हम `java.util.Date` और `java.time.LocalDate` का इस्तेमाल कर सकते हैं। यहां हम उक्त दोनों क्लासेज़ के मुख्य फंक्शन का उपयोग करते हुए एक उदाहरण देखेंगे। नीचे दिए गए कोड ब्लॉक में हम तीन तारीखों को तुलना करते हुए उनके बीच के दिनों और सालों को प्रिंट करेंगे।

```java
// तारीखों को तुलना करने के लिए दो विभिन्न ऑब्जेक्ट बनाएं
java.util.Date date1 = new java.util.Date(); // पहली तारीख
java.util.Date date2 = new java.util.Date(2020, 11, 27); // दूसरी तारीख

// तारीखों को मिलायें और उनकी तुलना करें
java.time.LocalDate localDate1 = date1.toInstant().atZone(ZoneId.systemDefault()).toLocalDate();
java.time.LocalDate localDate2 = java.time.LocalDate.now();

// उनके बीच के दिनों और सालों को प्रिंट करें
System.out.println("दोनों तारीखों के बीच " + localDate1.until(localDate2, java.time.temporal.ChronoUnit.DAYS) + " दिन हैं।");
System.out.println("दोनों तारीखों के बीच " + localDate1.until(localDate2, java.time.temporal.ChronoUnit.YEARS) + " साल हैं।");
```

उपरोक्त कोड का आउटपुट है: 

```
दोनों तारीखों के बीच