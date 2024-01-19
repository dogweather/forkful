---
title:                "स्ट्रिंग का अंतर्कलन"
html_title:           "Arduino: स्ट्रिंग का अंतर्कलन"
simple_title:         "स्ट्रिंग का अंतर्कलन"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/java/interpolating-a-string.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

भराई (interpolation) एक तरीका है जहां हम कोई भी मूल्य रेखांकित (interpolate) या एम्बेड हैं](embed) करते हैं. इसे उपयोग करके, हम स्ट्रिंग्स में डायनामिक मूल्यों को डाल सकते हैं. यह प्रोग्राम में कोड को पढ़ने, लिखने और डिबग करने को आसान बनाता है।


## कैसे करें:

Java में, हम `String.format()`	विधियों का उपयोग करके स्ट्रिंग में डायनामिक मूल्यों को interpolate करते हैं.

```Java
String name = "Raj";
String greeting = String.format("नमस्ते, %s!", name);
System.out.println(greeting);
```

उपरोक्त कोड निम्नलिखित आउटपुट देगा:

```
नमस्ते, राज!
```

## गहराई में:

Java में स्ट्रिंग इंटरपोलेशन के लिए ऐतिहासिक रूप से `String.format()` का इस्तेमाल किया जाता है। इसका विकल्प के तौर पर, Java 15 से Java ने Text Blocks और `formatted()` मेथड जोड़ा है। 

```Java
String name = "Raj";
String greeting = """
नमस्ते, %s!
""".formatted(name);
System.out.println(greeting);
```

नोट: `formatted()` मेथड का उपयोग करने के लिए आपके पास Java 15 या उससे अधिक होना चाहिए।

## और भी देखें:

1. [Java String Formatting (Oracle Java Documentation)](https://docs.oracle.com/javase/tutorial/java/data/numberformat.html)
2. [Java 15 Text Blocks (JEP 378)](https://openjdk.java.net/jeps/378)
3. [Java String Interpolation (Baeldung)](https://www.baeldung.com/java-string-interpolation-pattern)