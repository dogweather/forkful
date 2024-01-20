---
title:                "मानक त्रुटि में लिखना"
html_title:           "Arduino: मानक त्रुटि में लिखना"
simple_title:         "मानक त्रुटि में लिखना"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/java/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
जावा में, स्टैंडर्ड एरर (System.err) का इस्तेमाल एरर मैसेजेज दिखाने के लिए होता है। यह डेवलपर्स को कंसोल पर सिस्टम आउटपुट से एरर्स को अलग करके आसानी से डीबग करने में मदद करता है। 

## How to: (कैसे करें:)
यहां जावा में स्टैंडर्ड एरर में लिखने का उदाहरण है:

```java
public class StdErrExample {
    public static void main(String[] args) {
        // मानक आउटपुट पर मैसेज लिखना
        System.out.println("सब ठीक है");

        // मानक एरर पर एरर मैसेज लिखना
        System.err.println("यह एक एरर है");
    }
}
```

सैंपल आउटपुट:
```
सब ठीक है
यह एक एरर है
```

## Deep Dive (आधिक जानकारी)
पारंपरिक रूप से, स्टैंडर्ड एरर और स्टैंडर्ड आउटपुट दो अलग-अलग आउटपुट स्ट्रीम होते हैं। यह अलगाव आपको लॉगिंग और एरर मैसेजेस के लिए उन्हें अलग से हैंडल करने का मौका देता है। विकल्प के रूप में, लॉगिंग फ्रेमवर्क्स जैसे कि Log4j या SLF4J भी होते हैं जो लॉगिंग की ज्यादा सुविधाएं और नियंत्रण उपलब्ध कराते हैं। स्टैंडर्ड एरर सीधे ऑपरेटिंग सिस्टम के एरर हैंडलिंग मैकेनिज्म से जुड़ा हुआ है, जिससे इसे कंसोल, फाइल, या अन्य डेस्टिनेशंस पर पाइप किया जा सकता है।

## See Also (और देखें)
- Java's `System` class documentation: [Oracle Java Docs](https://docs.oracle.com/javase/8/docs/api/java/lang/System.html)
- Effective Java Logging with Log4j: [Log4j Tutorial](https://logging.apache.org/log4j/2.x/manual/)