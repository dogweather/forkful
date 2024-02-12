---
title:                "डीबग आउटपुट प्रिंट करना"
aliases:
- /hi/java/printing-debug-output.md
date:                  2024-01-20T17:52:54.043705-07:00
model:                 gpt-4-1106-preview
simple_title:         "डीबग आउटपुट प्रिंट करना"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/java/printing-debug-output.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)

डिबग आउटपुट प्रिंट करना, प्रोग्राम में वर्टिकल फ्लो को समझने और बग्स (गलतियों) को ढूंढने के लिए मैसेजेस को कंसोल पर दिखाता है। प्रोग्रामर्स इसका उपयोग वेरिएबल्स की वैल्यूज और सिस्टम की स्थिति को जानने के लिए करते हैं।

## How to: (कैसे करें:)

```Java
public class DebugExample {
    public static void main(String[] args) {
        int someVariable = 42;
        System.out.println("Debug: The value of someVariable is " + someVariable);
        
        if (someVariable > 10) {
            System.out.println("Debug: someVariable is greater than 10");
        }
    }
}
```
सैंपल आउटपुट:
```
Debug: The value of someVariable is 42
Debug: someVariable is greater than 10
```

## Deep Dive (गहराई से जानकारी)

डिबग आउटपुट की जड़ें प्रोग्रामिंग के शुरुआती दिनों में हैं, जब कंसोल ही प्राइमरी इंटरफेस हुआ करते थे। आज भी, `System.out.println()` जावा में डिबगिंग का मूल तरीका है और सरलता के कारण काफी पॉपुलर है। इसके अल्टरनेटिव्स में, लॉगिंग फ्रेमवर्क्स जैसे Log4j और SLF4J आते हैं जो अधिक फ्लेक्सिबिलिटी और कॉन्फ़िगुरेशन प्रदान करते हैं। 

`System.out.println()` सिस्टम आउटपुट स्ट्रीम के जरिए डायरेक्ट कंसोल पर टेक्स्ट प्रिंट करता है। इसका प्रयोग डेवेलपमेंट और टेस्टिंग फेज में तो होता ही है, पर रिलीज़्ड ऐप्लिकेशन में नहीं करना चाहिए, क्योंकि इससे परफॉर्मेंस पर प्रभाव पड़ता है और यह अनप्रोफेशनल लग सकता है।

## See Also (और जानें)

- [Java Logging Basics – Oracle Documentation](https://docs.oracle.com/javase/8/docs/technotes/guides/logging/index.html)
- [Log4j – Apache Logging Services](https://logging.apache.org/log4j/2.x/)
- [SLF4J Project Page](http://www.slf4j.org/)
- [Effective Java by Joshua Bloch – Debugging Tips](https://www.oreilly.com/library/view/effective-java/9780134686097/)
