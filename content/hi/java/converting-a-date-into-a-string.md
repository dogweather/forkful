---
title:    "Java: तारीख को स्ट्रिंग में रूपांतरण करना"
keywords: ["Java"]
---

{{< edit_this_page >}}

## क्यों

कभी-कभी हमें तारीखों को स्ट्रिंग में रूपांतरित करने की जरूरत होती है, जैसे कि उपयोगकर्ताओं को प्रदर्शित करने के लिए या डेटा को सहेजने के लिए। अगर आप जानते हैं कि इस प्रक्रिया को कैसे किया जाता है, तो आप अपने कोड को बेहतर ढंग से लिख सकते हैं और सिस्टम को सुदूर भविष्य में परिवर्तनों के लिए तैयार कर सकते हैं।

## कैसे

जावा कोड में तारीख को स्ट्रिंग में रूपांतरित करने के लिए हम `SimpleDateFormat` क्लास का इस्तेमाल कर सकते हैं। यहां हम एक उदाहरण प्रकाशित कर रहे हैं जो वर्तमान तारीख को "yyyy/MM/dd" फॉर्मेट में स्ट्रिंग में रूपांतरित करता है।

```Java
Date date = new Date();
SimpleDateFormat formatter = new SimpleDateFormat("yyyy/MM/dd");
String dateString = formatter.format(date);
System.out.println(dateString);
```

आउटपुट: 2021/08/05

## डीप डाइव

इस `SimpleDateFormat` क्लास को इस्तेमाल करने से पहले, हमें प्रथम तारीख के डेटा प्रकार को तय करना होता है, जैसे कि "dd/mm/yyyy" या "mm/dd/yyyy"। विभिन्न तारीख फॉर्मेट के लिए विभिन्न अक्षर उपलब्ध हैं, जो आप यहां देख सकते हैं। आप भी `Locale` पैरामीटर को इस्तेमाल कर सकते हैं जब आप स्ट्रिंग कोड को प्रकरण के दौरान उपयोग कर रहे हैं।

## देखें भी

- [SimpleDateFormat क्लास की जानकारी] (https://docs.oracle.com/javase/8/docs/api/java/text/SimpleDateFormat.html)
- [अधिक तारीख स्ट्रिंग फॉर्मेट विकल्प] (https://www.technicalkeeda.com/java-tutorials/java-simpledateformat-format-examples)

# देखें भी