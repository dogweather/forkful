---
title:                "Java: स्ट्रिंग की लंबाई का पता लगाना"
simple_title:         "स्ट्रिंग की लंबाई का पता लगाना"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/java/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## क्यों

कभी-कभी आपको प्रोग्रामिंग में यह जानने की जरूरत पड़ सकती है कि एक स्ट्रिंग के लंबाई को कैसे निर्धारित किया जाए। यह स्ट्रिंग के सामान्य संचालन से काफी अलग हो सकता है और इसलिए आपको इसे समझने के लिए एक गहराई से देखना होगा।

## कैसे

```Java
// एक स्ट्रिंग निर्धारित करें
String str = "Hello World";
// स्ट्रिंग की लंबाई जानने के लिए length() फ़ंक्शन का उपयोग करें
int length = str.length();
// आउटपुट: 11
System.out.println(length);
```

जैसा कि आप देख सकते हैं, हमने स्ट्रिंग के लिए `length()` फ़ंक्शन को कॉल करके उसकी लंबाई को निर्धारित किया है। इससे हमें उस स्ट्रिंग का लंबाई का माप लौटा गया है।

## गहराई में जाएं

स्ट्रिंग की लंबाई को निर्धारित करने के लिए यह फ़ंक्शन वास्तव में सभी स्ट्रिंगों के लिए है, जो कि `CharSequence` इंटरफ़ेस को इम्प्लिमेंट करते हैं। इसका मतलब है कि आप इस में कार्यवाही कर सकते हैं किसी भी स्ट्रिंग प्रकार के साथ जो ये इंटरफ़ेस देता है। इसके अलावा, यह फ़ंक्शन उस स्ट्रिंग के लिए काफी उपयोगी है जो कि डाइनामिक हो सकती है, जैसे उपयोगकर्ता से इनपुट लेना या अन्य स्रोतों से स्ट्रिंग प्राप्त करना।

## और भी देखें

[Java String Length Tutorial](https://www.javatpoint.com/java-string-length)

[Java String Class Documentation](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/lang/String.html)

[String Manipulation in Java](https://www.geeksforgeeks.org/string-manipulation-in-java-with-examples/)

यह लेख आपको स्ट्रिंग की लंब