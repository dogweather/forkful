---
date: 2024-01-26 01:08:33.617517-07:00
description: "\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902: \u092F\u0939\u093E\
  \u0901 \u091C\u093E\u0935\u093E \u092E\u0947\u0902 \u0932\u0949\u0917\u093F\u0902\
  \u0917 \u0936\u0941\u0930\u0941 \u0915\u0930\u0928\u0947 \u0915\u0947 \u0932\u093F\
  \u090F \u0938\u0930\u0932 \u0924\u0930\u0940\u0915\u093E \u0926\u093F\u092F\u093E\
  \ \u0917\u092F\u093E \u0939\u0948 `java.util.logging` \u092A\u0948\u0915\u0947\u091C\
  \ \u0915\u093E \u0909\u092A\u092F\u094B\u0917 \u0915\u0930\u0924\u0947 \u0939\u0941\
  \u090F, \u091C\u094B \u0915\u093F \u091C\u093E\u0935\u093E \u092E\u0947\u0902 \u092C\
  \u093F\u0932\u094D\u091F-\u0907\u0928 \u0939\u0948\u0964."
lastmod: '2024-03-13T22:44:52.124850-06:00'
model: gpt-4-1106-preview
summary: "\u092F\u0939\u093E\u0901 \u091C\u093E\u0935\u093E \u092E\u0947\u0902 \u0932\
  \u0949\u0917\u093F\u0902\u0917 \u0936\u0941\u0930\u0941 \u0915\u0930\u0928\u0947\
  \ \u0915\u0947 \u0932\u093F\u090F \u0938\u0930\u0932 \u0924\u0930\u0940\u0915\u093E\
  \ \u0926\u093F\u092F\u093E \u0917\u092F\u093E \u0939\u0948 `java.util.logging` \u092A\
  \u0948\u0915\u0947\u091C \u0915\u093E \u0909\u092A\u092F\u094B\u0917 \u0915\u0930\
  \u0924\u0947 \u0939\u0941\u090F, \u091C\u094B \u0915\u093F \u091C\u093E\u0935\u093E\
  \ \u092E\u0947\u0902 \u092C\u093F\u0932\u094D\u091F-\u0907\u0928 \u0939\u0948\u0964\
  ."
title: "\u0932\u0949\u0917\u093F\u0902\u0917"
weight: 17
---

## कैसे करें:
यहाँ जावा में लॉगिंग शुरु करने के लिए सरल तरीका दिया गया है `java.util.logging` पैकेज का उपयोग करते हुए, जो कि जावा में बिल्ट-इन है।

```java
import java.util.logging.Logger;
import java.util.logging.Level;

public class AppLogging {
    private final static Logger LOGGER = Logger.getLogger(Logger.GLOBAL_LOGGER_NAME);

    public static void main(String[] args) {
        LOGGER.info("Logging an INFO-level message");

        try {
            int division = 10 / 0;
        } catch (ArithmeticException e) {
            LOGGER.log(Level.SEVERE, "Exception occur", e);
        }
    }
}
```

इससे निम्नलिखित प्रकार का आउटपुट प्राप्त होगा:

```
Jul 03, 2023 2:00:00 PM AppLogging main
INFO: Logging an INFO-level message
Jul 03, 2023 2:00:00 PM AppLogging main
SEVERE: Exception occur
java.lang.ArithmeticException: / by zero
    at AppLogging.main(AppLogging.java:10)
```

## गहराई से जानिए
जावा में लॉगिंग काफी बदल चुकी है। प्राचीन समय में, लॉगिंग अधिक यादृच्छिक थी जैसे सिस्टम आउटपुट्स और स्वयं लिखित तंत्र के साथ। हालांकि, मानकीकरण की आवश्यकता ने `Log4j` और `SLF4J` जैसे लॉगिंग APIs की ओर अग्रसर किया। `java.util.logging` पैकेज को JDK 1.4 में पेश किया गया था, जिसने संदेशों को लॉग करने के लिए एक मानकीकृत तरीका प्रदान किया।

`java.util.logging` (JUL) के विकल्पों में Log4j 2 और SLF4J शामिल हैं। जहाँ JUL जावा में निर्मित होता है और इसके लिए अतिरिक्त निर्भरताओं की आवश्यकता नहीं होती, वहीं Log4j 2 और SLF4J अधिक उन्नत सुविधाएँ जैसे कि लॉगिंग कॉन्फ़िगरेशन पर अधिक विस्तृत नियंत्रण, असिंक्रोनस लॉगिंग और बेहतर प्रदर्शन प्रदान करते हैं।

कार्यान्वयन के दृष्टिकोण से, लॉगिंग या तो सिंक्रोनस हो सकती है, जहाँ प्रत्येक लॉग संदेश उस थ्रेड में संसाधित होता है जिसने उसे जनरेट किया, या असिंक्रोनस, जहाँ संदेशों को एक अलग थ्रेड को सौंप दिया जाता है। असिंक्रोनस लॉगिंग प्रदर्शन को बेहतर बना सकती है लेकिन जटिलता पैदा करती है क्योंकि किसी को समानांतरता को संभालना पड़ता है और सुनिश्चित करना पड़ता है कि एप्लीकेशन क्रैश पर लॉग संदेश खो न जाएं।

## इसे भी देखें
- [Log4j 2](https://logging.apache.org/log4j/2.x/)
- [SLF4J](http://www.slf4j.org/)
- [ओरेकल का आधिकारिक लॉगिंग ओव्हरव्यू](https://docs.oracle.com/javase/8/docs/technotes/guides/logging/overview.html)
- [java.util.logging पर ट्यूटोरियल](https://www.vogella.com/tutorials/Logging/article.html)
