---
date: 2024-01-20 17:46:04.109850-07:00
description: "How to: (\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902:) `substring()`\
  \ \u092E\u0947\u0925\u0921 Java \u092E\u0947\u0902 \u092C\u0939\u0941\u0924 \u092A\
  \u0939\u0932\u0947 \u0938\u0947 \u0939\u0948, \u0914\u0930 \u092F\u0947 String class\
  \ \u0915\u093E \u090F\u0915 \u092E\u0939\u0924\u094D\u0935\u092A\u0942\u0930\u094D\
  \u0923 \u0939\u093F\u0938\u094D\u0938\u093E \u0930\u0939\u093E \u0939\u0948\u0964\
  \ \u091C\u0948\u0938\u0947-\u091C\u0948\u0938\u0947 Java \u0905\u092A\u0917\u094D\
  \u0930\u0947\u0921 \u0939\u094B\u0924\u093E \u0917\u092F\u093E,\u2026"
lastmod: '2024-04-05T22:51:06.779015-06:00'
model: gpt-4-1106-preview
summary: "(\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902:) `substring()` \u092E\
  \u0947\u0925\u0921 Java \u092E\u0947\u0902 \u092C\u0939\u0941\u0924 \u092A\u0939\
  \u0932\u0947 \u0938\u0947 \u0939\u0948, \u0914\u0930 \u092F\u0947 String class \u0915\
  \u093E \u090F\u0915 \u092E\u0939\u0924\u094D\u0935\u092A\u0942\u0930\u094D\u0923\
  \ \u0939\u093F\u0938\u094D\u0938\u093E \u0930\u0939\u093E \u0939\u0948\u0964 \u091C\
  \u0948\u0938\u0947-\u091C\u0948\u0938\u0947 Java \u0905\u092A\u0917\u094D\u0930\u0947\
  \u0921 \u0939\u094B\u0924\u093E \u0917\u092F\u093E, `substring()` \u092E\u0947\u0902\
  \ \u092D\u0940 \u0938\u0941\u0927\u093E\u0930 \u0939\u094B\u0924\u0947 \u0930\u0939\
  \u0947, \u091C\u093F\u0938\u0938\u0947 \u092F\u0947 \u0914\u0930 \u092D\u0940 \u0915\
  \u0941\u0936\u0932 \u0939\u094B \u0917\u092F\u093E\u0964 \u0905\u0932\u0917-\u0905\
  \u0932\u0917 \u0924\u0930\u0940\u0915\u094B\u0902 \u092E\u0947\u0902 `StringBuffer`\
  \ \u0914\u0930 `StringBuilder` \u091C\u0948\u0938\u0947 \u0915\u094D\u0932\u093E\
  \u0938\u0947\u0938 \u0915\u093E \u092D\u0940 \u0907\u0938\u094D\u0924\u0947\u092E\
  \u093E\u0932 \u0939\u094B\u0924\u093E \u0939\u0948, \u091C\u0948\u0938\u0947 `substring()`\
  \ \u0915\u093E \u0907\u0938\u094D\u0924\u0947\u092E\u093E\u0932 \u0915\u0930\u0915\
  \u0947 \u092D\u0940 \u0915\u0930 \u0938\u0915\u0924\u0947 \u0939\u0948\u0902\u0964\
  \ \u0907\u0902\u092A\u094D\u0932\u0940\u092E\u0947\u0902\u091F\u0947\u0936\u0928\
  \ \u0921\u093F\u091F\u0947\u0932\u094D\u0938 \u0915\u0940 \u092C\u093E\u0924 \u0915\
  \u0930\u0947\u0902 \u0924\u094B, \u092A\u0941\u0930\u093E\u0928\u0947 Java \u0935\
  \u0930\u094D\u091C\u0928\u094D\u0938 \u092E\u0947\u0902 `substring()` \u0915\u093E\
  \ \u0907\u0938\u094D\u0924\u0947\u092E\u093E\u0932 \u0925\u094B\u0921\u093C\u093E\
  \ \u0938\u0902\u0938\u093E\u0927\u0928 \u0917\u0939\u0928 \u0939\u094B\u0924\u093E\
  \ \u0925\u093E \u0915\u094D\u092F\u094B\u0902\u0915\u093F \u092F\u0939 \u0928\u090F\
  \ \u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917 \u0915\u0947 \u0932\u093F\u090F\
  \ \u0939\u092E\u0947\u0936\u093E \u0928\u0908 memory \u0905\u0932\u0949\u0915\u0947\
  \u091F \u0915\u0930\u0924\u093E \u0925\u093E\u0964."
title: "\u0938\u092C\u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917\u094D\u0938 \u0928\
  \u093F\u0915\u093E\u0932\u0928\u093E"
weight: 6
---

## How to: (कैसे करें:)
```java
public class SubstringExample {
    public static void main(String[] args) {
        String str = "HelloWorld";
        // पहले से चौथे करैक्टर तक का सबस्ट्रिंग
        String sub1 = str.substring(1, 5); // "ello"
        // छठे करैक्टर से आखिर तक का सबस्ट्रिंग
        String sub2 = str.substring(5); // "World"
        
        System.out.println("Substring 1: " + sub1);
        System.out.println("Substring 2: " + sub2);
    }
}
```

आउटपुट:
```
Substring 1: ello
Substring 2: World
```

## Deep Dive (गहराई से जानकारी):
`substring()` मेथड Java में बहुत पहले से है, और ये String class का एक महत्वपूर्ण हिस्सा रहा है। जैसे-जैसे Java अपग्रेड होता गया, `substring()` में भी सुधार होते रहे, जिससे ये और भी कुशल हो गया। अलग-अलग तरीकों में `StringBuffer` और `StringBuilder` जैसे क्लासेस का भी इस्तेमाल होता है, जैसे `substring()` का इस्तेमाल करके भी कर सकते हैं। इंप्लीमेंटेशन डिटेल्स की बात करें तो, पुराने Java वर्जन्स में `substring()` का इस्तेमाल थोड़ा संसाधन गहन होता था क्योंकि यह नए स्ट्रिंग के लिए हमेशा नई memory अलॉकेट करता था।

## See Also (इसे भी देखें):
- Oracle Java Documentation: https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/lang/String.html#substring(int,int)
- Java String tutorial from w3schools: https://www.w3schools.com/java/java_strings.asp
- Java String `substring()` method from GeeksforGeeks: https://www.geeksforgeeks.org/substring-in-java/
