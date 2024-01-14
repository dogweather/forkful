---
title:                "Arduino: नियमित अभिव्यक्तियों का उपयोग"
simple_title:         "नियमित अभिव्यक्तियों का उपयोग"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/arduino/using-regular-expressions.md"
---

{{< edit_this_page >}}

## क्यों

रेगुलर एक्सप्रेशन्स का प्रयोग करके आर्डुइनो प्रोग्रामिंग सीखना आपके कोड को स्वचालित करने और इसे अधिक सुविधाजनक बनाने में मदद कर सकता है।

## कैसे करें

रेगुलर एक्सप्रेशन्स को आर्डुइनो में इस्तेमाल करने के लिए पहले आपको एक लाइब्रेरी जोड़नी होगी। आप ```Arduino``` के कोड ब्लॉक के अंदर ```#include <RegexPattern.h> ```का उपयोग करके लाइब्रेरी को जोड़ सकते हैं। नीचे दिए गए कोड ब्लॉक में हम एक रेगुलर एक्सप्रेशन का उपयोग करके एक स्ट्रिंग से नंगे शब्दों को हटा सकते हैं।

```Arduino
#include <RegexPattern.h>

String str = "Hello, World! How are you?";
RegexPattern pattern("[^a-zA-Z0-9 ]");

String result = RegexReplace(str, pattern, "");

// Output: Hello World How are you
```

## गहराई से जाने

रेगुलर एक्सप्रेशन्स काफी मजबूत और परिश्यापेक्षी होते हैं। आप इनका उपयोग करके किसी भी तरह के संख्याओं, वर्णमालाओं या अन्य कैरेक्टर्स को ढूंढ सकते हैं और उन्हें प्रसंस्करण कर सकते हैं। इसके साथ ही आप स्ट्रिंग के साथ काफी मजबूत पैटर्न मैचिंग भी कर सकते हैं जो आपके कोड को और अधिक उच्च स्तर पर ले जाएगी।

## देखें भी

- [Arduino लाइब्रेरी डाउनलोड](https://www.arduino.cc/en/Reference/RegexPattern)
- [ऑनलाइन रेगुलर एक्सप्रेशन एडिटर](https://regex101.com/)
- [Arduino वेबसाइट](https://www.arduino.cc/)