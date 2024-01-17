---
title:                "दो तारीखों की तुलना करना"
html_title:           "Java: दो तारीखों की तुलना करना"
simple_title:         "दो तारीखों की तुलना करना"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/java/comparing-two-dates.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
दो तारीखों को तुलना करना एक प्रोग्रामिंग में सामान्य क्रिया है, जिसमें हम दो विभिन्न दिनांकों को तुलना करके दोनों की तारीख के बीच कैसा अंतर है यह जानना चाहते हैं। कई बार, हमारे प्रोग्राम में दो तारीखों को मिलाकर कुछ विशिष्ट कार्यों को करने की आवश्यकता होती है, जहां हमें दो तारीखों के बीच का अंतर निकालने की जरूरत पड़ती है।

## कैसे करें?
तारीखों को तुलना करने के लिए ```Java``` में हम तीन तरीकों का इस्तेमाल कर सकते हैं। पहला तरीका है ```isEqualTo()```, जो दो तारीखों को तुलना करता है और यदि वे बराबर होते हैं तो ```true``` लौटाता है। दूसरा तरीका है ```isAfter()```, जो दो तारीखों को तुलना करता है और दूसरी तारीख पहली तारीख के बाद है तो ```true``` लौटाता है। तीसरा तरीका है ```isBefore()```, जो दो तारीखों को तुलना करता है और पहली तारीख दूसरी तारीख से पहले है तो ```true``` लौटाता है। नीचे दिए गए उदाहरण में हम इन तीनों तरीकों का उपयोग करके दो तारीखों के बीच का अंतर निकालेंगे।

```Java
import java.time.LocalDate;

// isEqualTo()
LocalDate date1 = LocalDate.of(2021, 01, 01);
LocalDate date2 = LocalDate.of(2021, 01, 01);
System.out.println(date1.isEqualTo(date2)); // Output: true

// isAfter()
LocalDate date3 = LocalDate.of(2021, 01, 01);
LocalDate date4 = LocalDate.of(2020, 12, 31);
System.out.println(date3.isAfter(date4)); // Output: true

// isBefore()
LocalDate date5 = LocalDate.of(2021, 01, 01);
LocalDate date6 = LocalDate.of(2021, 01, 02);
System.out.println(date5.isBefore(date6)); // Output: true
```

## गहराई में जाएं
जब हम तारीखों को तुलना करते हैं, तो हम विभिन्न तारीख प्रकार जैसे ```java.util.Date``` और ```java.time.LocalDate``` को इस्तेमाल कर सकते हैं। पहले, ```java.util.Date``` को इस्तेमाल कर रहे थे, जो ```java.time.LocalDate``` से निष्क्रिय हो गया है। इसके साथ सामान्य समस्या है कि उसमें एक्स्ट्रा समय जानकारी होती है जो तारीख को निगारन करती है। ```java.time.LocalDate``` गुणवत्ता को बेहतर तरीके से निगार सकता है और यह तारीखों को तुलना करने के लिए सुविधाजनक है। भविष्य में, यह उपाय ```joda.time``` के साथ उपलब्ध भी हो सकता है जो एक बेहतर तारीख प्रकार प्रदान करता है और भारत के घड़ियाल के समस्या को हल कर सकता है।

## लिंक्स देखें
अधिक जानकारी