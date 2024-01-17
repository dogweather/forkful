---
title:                "उपसामग्री निकालना"
html_title:           "Java: उपसामग्री निकालना"
simple_title:         "उपसामग्री निकालना"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/java/extracting-substrings.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
जब एक प्रोग्रामर अपने कोड में एक स्ट्रिंग का एक उपस्टरिंग निकालता है, तो वह स्ट्रिंग के कुछ हिस्सों को अलग ढंग से प्रदर्शित करता है। इसे स्ट्रिंग का सबस्ट्रिंग निकालना कहा जाता है। कई प्रोग्रामिंग स्थितियों में, यह स्ट्रिंग का उपयोग आसानी से किया जा सकता है और कोड को अधिक स्पष्ट बनाता है।

## कैसे:
जावा में स्ट्रिंग का सबस्ट्रिंग निकलने के लिए substring() मेथड का उपयोग किया जा सकता है। इसमें दो पैरामीटर होते हैं - शुरुआती और अंतिम इंडेक्स। उदाहरण के लिए: 

```java 
String str = "Hello World!";
String subStr = str.substring(0, 5);
System.out.println(subStr);

// Output: Hello
```

यहां, हमने 0 से शुरू होने वाले प्रथम 5 इंडेक्स को उपयोग करके स्ट्रिंग "Hello World!" का पहला शब्द निकाला है।

## गहराई में खोजें:
इस तकनीक की शुरुआत 1976 में यूएसबी में दर्ज एक विशेषता थी। आज के समय में भी, अधिकांश प्रोग्रामिंग भाषाओं में स्ट्रिंग का सबस्ट्रिंग निकालने का यही तरीका है। उनमें से एक है C और C++ में। इसके अलावा, पाइथन में भी स्ट्रिंग का सबस्ट्रिंग निकालने के लिए substring() मेथड मौजूद है। स्वचालित रूप से स्ट्रिंग की लंबाई प्राप्त करने या विशिष्ट शब्दों को तालिका में बदलने के लिए भी substring() का उपयोग किया जाता है।

## और देखें:
यदि आपको स्ट्रिंग का सबस्ट्रिंग निकालने के बारे में और जानना है, तो नीचे दिए गए स्रोतों को जरूर देखें:

- [जावा substring() डॉक्यूमेंटेशन](https://docs.oracle.com/javase/7/docs/api/java/lang/String.html#substring(int, int))
- [C में स्ट्रिंग सम्बंधित (Substring-related) फ़ंक्शंस](https://www.programiz.com/c-programming/library-function/string.h/strstr)
- [पाइथन substring() फ़ंक्शंस](https://www.programiz.com/python-programming/methods/string/substring)