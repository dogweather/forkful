---
title:    "Java: स्ट्रिंग को लोअर केस में रूपांतरण करना"
keywords: ["Java"]
---

{{< edit_this_page >}}

## इसे क्यों?

प्रोग्रामिंग में स्ट्रिंग ऑपरेशन्स बहुत अधिक प्रयोग किए जाते हैं और इनमें से एक है स्ट्रिंग को लोअर केस में कनवर्ट करना। इससे आप स्ट्रिंग में हुए गलतियों को सुधार सकते हैं और स्ट्रिंगों को अपनी आवश्यकतानुसार सोर्ट कर सकते हैं।

## कैसे करें?

```java
// एक स्ट्रिंग डेक्लेर करें
String myString = "Hello World";

// स्ट्रिंग को लोअर केस में कनवर्ट करें
String lowerCaseString = myString.toLowerCase();

// नतीजे को प्रिंट करें
System.out.println(lowerCaseString);

// आउटपुट: hello world
```

## गहराई में खोजें

स्ट्रिंग को लोअर केस में कनवर्ट करना एक अनिवार्य चरण है जो आपको स्ट्रिंग ऑपरेशन्स में आरामदायक बनाता है। इसमें आपको स्ट्रिंग ऑब्जेक्ट की अक्षरों को छोटे अक्षर में परिवर्तित करने के लिए String क्लास में उपलब्ध बिल्ट-इन फंक्शन का उपयोग करना पड़ता है। इस फंक्शन का उपयोग करने से आप स्ट्रिंग के वर्णमाला में विभिन्न अक्षरों को सामान्य सभों में बदल सकते हैं।

## देखिये

- [Java String class documentation](https://docs.oracle.com/javase/7/docs/api/java/lang/String.html)
- [Tutorial: Manipulating Strings in Java](https://www.baeldung.com/java-string)
- [Video: String Operations in Java](https://www.youtube.com/watch?v=8Jxjr3Gvh_k)