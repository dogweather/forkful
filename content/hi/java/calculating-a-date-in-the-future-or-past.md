---
title:    "Java: भविष्य या अतीत में एक तारीख की गणना"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/hi/java/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## क्यों

दिनांक को भविष्य या भूत का गणना क्यों किया जाता है, इसका पता मतलब होगा।

## कैसे करें

यदि आप जावा में दिनांक को भविष्य या भूत के रूप में गणना करना चाहते हैं, तो आपको ```Java ... ``` कोड ब्लॉक में कोडिंग उदाहरण और नमूना आउटपुट के साथ आगे बढ़ना होगा।

```java
// वर्तमान दिनांक प्राप्त करें
LocalDate today = LocalDate.now();

// भविष्य की तारीख का गणना करें, यहां हम 30 दिन बाद की तारीख प्राप्त कर रहे हैं
LocalDate after30Days = today.plusDays(30);

// नमूना आउटपुट प्रिंट करें
System.out.println("आज की तारीख: " + today);
System.out.println("30 दिनों के बाद की तारीख: " + after30Days);
```

आउटपुट:

```java
आज की तारीख: 2021-05-17
30 दिनों के बाद की तारीख: 2021-06-16
```

## गहराई में जाएं

दिनांक को भविष्य या भूत का गणना करने के बारे में और गहराई से जानने के लिए, आपको यह जानना होगा कि जावा में दिनांक को कैसे जोड़ा जाता है और कैसे काम करता है। इसके अलावा, यह कैसे मानया जाता है कि खुले रेंज और बंद रेंज में फर्क क्या होता है और यह दिनांक को बदल सकता है। आगे बढ़ने से पहले हमें दिनांक के स्त्रोत को समझना भी जरूरी है।

## नज़र्मंदित देखें

[JavaDocs: LocalDate](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/time/LocalDate.html)

[Java Tutorials: Date Time API](https://docs.oracle.com/javase/tutorial/datetime/overview/index.html)

[Java8 के नवीनतम अद्यतन से जुड़े ट्यूटोरियल](https://www.javatpoint.com/java8)

## देखा ज