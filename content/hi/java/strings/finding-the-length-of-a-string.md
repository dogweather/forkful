---
date: 2024-01-20 17:48:03.089308-07:00
description: "\u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917 \u0915\u0940 \u0932\
  \u0902\u092C\u093E\u0908 \u091C\u093E\u0928\u0928\u093E \u092E\u0924\u0932\u092C\
  \ \u0939\u0948 \u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917 \u092E\u0947\u0902\
  \ \u0915\u093F\u0924\u0928\u0947 \u0915\u0948\u0930\u0947\u0915\u094D\u091F\u0930\
  \u094D\u0938 \u0939\u0948\u0902\u0964 \u092A\u094D\u0930\u094B\u0917\u094D\u0930\
  \u093E\u092E\u0930\u094D\u0938 \u0907\u0938\u0947 \u0907\u0938\u0932\u093F\u090F\
  \ \u0915\u0930\u0924\u0947 \u0939\u0948\u0902 \u0924\u093E\u0915\u093F \u0935\u0947\
  \ \u0921\u093E\u091F\u093E \u0915\u094B \u092E\u0948\u0928\u0947\u091C \u0915\u0930\
  \ \u0938\u0915\u0947\u0902, \u0935\u0948\u0932\u093F\u0921\u0947\u0936\u0928 \u091A\
  \u0947\u0915 \u0915\u0930 \u0938\u0915\u0947\u0902, \u0914\u0930\u2026"
lastmod: '2024-03-13T22:44:52.098126-06:00'
model: gpt-4-1106-preview
summary: "\u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917 \u0915\u0940 \u0932\u0902\
  \u092C\u093E\u0908 \u091C\u093E\u0928\u0928\u093E \u092E\u0924\u0932\u092C \u0939\
  \u0948 \u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917 \u092E\u0947\u0902 \u0915\
  \u093F\u0924\u0928\u0947 \u0915\u0948\u0930\u0947\u0915\u094D\u091F\u0930\u094D\u0938\
  \ \u0939\u0948\u0902\u0964 \u092A\u094D\u0930\u094B\u0917\u094D\u0930\u093E\u092E\
  \u0930\u094D\u0938 \u0907\u0938\u0947 \u0907\u0938\u0932\u093F\u090F \u0915\u0930\
  \u0924\u0947 \u0939\u0948\u0902 \u0924\u093E\u0915\u093F \u0935\u0947 \u0921\u093E\
  \u091F\u093E \u0915\u094B \u092E\u0948\u0928\u0947\u091C \u0915\u0930 \u0938\u0915\
  \u0947\u0902, \u0935\u0948\u0932\u093F\u0921\u0947\u0936\u0928 \u091A\u0947\u0915\
  \ \u0915\u0930 \u0938\u0915\u0947\u0902, \u0914\u0930\u2026"
title: "\u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917 \u0915\u0940 \u0932\u0902\
  \u092C\u093E\u0908 \u091C\u094D\u091E\u093E\u0924 \u0915\u0930\u0928\u093E"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
स्ट्रिंग की लंबाई जानना मतलब है स्ट्रिंग में कितने कैरेक्टर्स हैं। प्रोग्रामर्स इसे इसलिए करते हैं ताकि वे डाटा को मैनेज कर सकें, वैलिडेशन चेक कर सकें, और लूप्स को सही से चला सकें।

## How to: (कैसे करें:)
```java
public class StringLengthExample {
    public static void main(String[] args) {
        String greeting = "नमस्ते दुनिया!";
        int length = greeting.length();
        System.out.println("स्ट्रिंग की लंबाई: " + length);
    }
}
```
आउटपुट:
```
स्ट्रिंग की लंबाई: 14
```

## Deep Dive (गहराई से समझें)
पहले के जावा वर्जन्स में `length()` मेथड `String` ऑब्जेक्ट के लिए बहुत महत्वपूर्ण था क्योंकि वह ही बताता था की स्ट्रिंग में कितने कैरेक्टर्स हैं। अब भी यह महत्वपूर्ण है, पर नए फंक्शंस और एपीआईस भी हैं जैसे कि `codePoints()` जो Unicode कोड पॉइंट्स को सही तरीके से गिनता है।

स्ट्रिंग की लंबाई निकालने की इम्प्लीमेंटेशन की बात करें तो, JVM (Java Virtual Machine) स्ट्रिंग की इंटर्नल एरे की `length` प्रॉपर्टी का इस्तेमाल करती है। सिर्फ ASCII कैरेक्टर्स के लिए स्ट्रिंग की लंबाई और उनकी संख्या एक समान होती है, लेकिन यूनिकोड के लिए, वास्तविक कैरेक्टर काउंट अलग हो सकता है।

## See Also (देखें भी)
- [Java String documentation](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/lang/String.html)
- [Oracle Java Tutorials](https://docs.oracle.com/javase/tutorial/)
