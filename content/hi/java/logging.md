---
title:                "लॉगिंग"
date:                  2024-01-26T01:08:33.617517-07:00
model:                 gpt-4-1106-preview
simple_title:         "लॉगिंग"

category:             "Java"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/java/logging.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
लॉगिंग मूल्तः सॉफ्टवेयर एप्लीकेशन के भीतर होने वाली घटनाओं को रिकॉर्ड करने की प्रक्रिया है। प्रोग्रामर्स इन घटनाओं को रनटाइम जानकारी कैप्चर करने, समस्याओं का डिबग करने, सिस्टम व्यवहार की मॉनिटरिंग करने, और सुरक्षा एवं अनुपालन के उद्देश्यों के लिए ऑडिट ट्रेल बनाने के लिए लॉग करते हैं।

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
