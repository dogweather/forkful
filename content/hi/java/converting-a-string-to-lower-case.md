---
title:                "एक स्ट्रिंग को लोअर केस में परिवर्तित करना"
html_title:           "Kotlin: एक स्ट्रिंग को लोअर केस में परिवर्तित करना"
simple_title:         "एक स्ट्रिंग को लोअर केस में परिवर्तित करना"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/java/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## क्या है और इसका महत्व क्या है? ("What & Why?")

एक स्ट्रिंग को छोटे अक्षरों में बदलना (converting a string to lower case) का मतलब होता है कि हम सभी बड़े अक्षरों को उनके छोटे संस्करणों में बदल देते हैं। प्रोग्रामर्स इसे क्यों करते हैं? क्योंकि यह कस्टम डाटा प्रोसेसिंग, जैसे कि स्ट्रिंग मेल करना और खोजना, को आसान बना देता है। 

## कैसे करें: ("How to:")

Java में, हम `toLowerCase()` फ़ंक्शन का उपयोग करके एक स्ट्रिंग को छोटे अक्षरों में बदल सकते हैं।

```java
String str = "HELLO WORLD!";
str = str.toLowerCase();
System.out.println(str);
```
Output:
```
hello world!
```
यह कोड "HELLO WORLD!"को "hello world!" में बदल देता है।

## गहरी जानकारी ("Deep Dive")

1. **ऐतिहासिक प्रसंग** : `toLowerCase()` फ़ंक्शन Java 1.0 से मौजूद रहा है। यह हमें बिना किसी विशेष लॉजिक के एक स्ट्रिंग को छोटे अक्षरों में बदलने की सुविधा देता है।

2. **विकल्प** : केस-इंसेंसिटिव कम्पेरिसन के लिए, `equalsIgnoreCase()` भी एक विकल्प हो सकता है। यह स्ट्रिंग्स को छोटे अक्षरों में बदलने की आवश्यकता को समाप्त कर देता है। 

3. **विस्तार से समझना** : `toLowerCase()` का कार्यण्वयन Unicode का अनुसरण करता है, जिसका मतलब है कि यह सभी भाषाओं के छोटे अक्षरों के नियमों का सम्मान करता है। यहाँ कहने का मतलब है कि हम इंग्लिश के अलावा दूसरी भाषाओं के स्ट्रिंग्स को भी हंडल करने में सक्षम होंगे।

## और देखें ("See Also")

1. [compareToIgnoreCase and equalsIgnoreCase in Java](https://www.tutorialspoint.com/difference-between-comparetoignorecase-and-equalsignorecase-method-in-java)
2. [Java String toLowerCase() Method with examples](https://www.javatpoint.com/java-string-tolowercase)

इसे आशा है कि आपको इस लेख का निर्माण और इस महत्वपूर्ण फ़ंक्शन का विश्लेषण पसंद आया होगा।  अगर आपके पास ऐसा कुछ और है जो आप जानना चाहते हैं, तो कृपया टिप्पणी करें या हमें पोस्ट करें।