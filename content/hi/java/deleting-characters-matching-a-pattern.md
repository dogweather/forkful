---
title:    "Java: पैटर्न से मेल खाने वाले अक्षरों को हटाना"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/hi/java/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

# क्यों

एक व्यक्ति को पैटर्न को मैच करने वाले अक्षरों को हटाने में रुचि रखने के सिर्फ 1-2 सेंटेंसेज़ का व्याख्यान करें.

# कैसे करें

कोडिंग उदाहरण और "```Java ... ```" कोड ब्लॉक के भीतर नमूना आउटपुट सहित दिखाएं.

```Java
// एक सामान्य स्ट्रिंग बनाएं
String str = "aabccdee";

//पैटर्न दर्ज करें
Pattern pattern = Pattern.compile("c*d");

// मैचिंग अक्षरों को हटाएं
String result = str.replaceAll(pattern, "");

// आउटपुट दिखाएं
System.out.println(result);

// आउटपुट: aabee
```

# गहराई में जाएं

पैटर्न को मैच करने वाले अक्षरों को हटाने का यह प्रक्रिया आमतौर पर इस्तेमाल किया जाता है जब एक स्ट्रिंग में से कई अक्षर या शब्द हटाने की आवश्यकता होती है. यह बहुत सरल और समय बचाने वाली होती है जब आप बड़े डेटासेट्स के साथ काम कर रहे होते हैं.

# देखें भी

- [String 클래스 डॉक्यूमेंटेशन](https://docs.oracle.com/javase/7/docs/api/java/lang/String.html)
- [Java में Substring कैसे हटाएं](https://www.geeksforgeeks.org/java-lang-stringbuffer-substring-method-in-java/) 
- [Java में विभिन्न प्रकार के पैटर्न के लिए स्ट्रिंग मैचिंग](https://www.javatpoint.com/java-regex)