---
title:                "स्ट्रिंग की लंबाई ज्ञात करना"
date:                  2024-01-20T17:48:03.089308-07:00
model:                 gpt-4-1106-preview
simple_title:         "स्ट्रिंग की लंबाई ज्ञात करना"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/java/finding-the-length-of-a-string.md"
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
