---
date: 2024-01-20 17:52:54.043705-07:00
description: "\u0921\u093F\u092C\u0917 \u0906\u0909\u091F\u092A\u0941\u091F \u092A\
  \u094D\u0930\u093F\u0902\u091F \u0915\u0930\u0928\u093E, \u092A\u094D\u0930\u094B\
  \u0917\u094D\u0930\u093E\u092E \u092E\u0947\u0902 \u0935\u0930\u094D\u091F\u093F\
  \u0915\u0932 \u092B\u094D\u0932\u094B \u0915\u094B \u0938\u092E\u091D\u0928\u0947\
  \ \u0914\u0930 \u092C\u0917\u094D\u0938 (\u0917\u0932\u0924\u093F\u092F\u094B\u0902\
  ) \u0915\u094B \u0922\u0942\u0902\u0922\u0928\u0947 \u0915\u0947 \u0932\u093F\u090F\
  \ \u092E\u0948\u0938\u0947\u091C\u0947\u0938 \u0915\u094B \u0915\u0902\u0938\u094B\
  \u0932 \u092A\u0930 \u0926\u093F\u0916\u093E\u0924\u093E \u0939\u0948\u0964 \u092A\
  \u094D\u0930\u094B\u0917\u094D\u0930\u093E\u092E\u0930\u094D\u0938 \u0907\u0938\u0915\
  \u093E \u0909\u092A\u092F\u094B\u0917\u2026"
lastmod: '2024-03-13T22:44:52.118014-06:00'
model: gpt-4-1106-preview
summary: "\u0921\u093F\u092C\u0917 \u0906\u0909\u091F\u092A\u0941\u091F \u092A\u094D\
  \u0930\u093F\u0902\u091F \u0915\u0930\u0928\u093E, \u092A\u094D\u0930\u094B\u0917\
  \u094D\u0930\u093E\u092E \u092E\u0947\u0902 \u0935\u0930\u094D\u091F\u093F\u0915\
  \u0932 \u092B\u094D\u0932\u094B \u0915\u094B \u0938\u092E\u091D\u0928\u0947 \u0914\
  \u0930 \u092C\u0917\u094D\u0938 (\u0917\u0932\u0924\u093F\u092F\u094B\u0902) \u0915\
  \u094B \u0922\u0942\u0902\u0922\u0928\u0947 \u0915\u0947 \u0932\u093F\u090F \u092E\
  \u0948\u0938\u0947\u091C\u0947\u0938 \u0915\u094B \u0915\u0902\u0938\u094B\u0932\
  \ \u092A\u0930 \u0926\u093F\u0916\u093E\u0924\u093E \u0939\u0948\u0964 \u092A\u094D\
  \u0930\u094B\u0917\u094D\u0930\u093E\u092E\u0930\u094D\u0938 \u0907\u0938\u0915\u093E\
  \ \u0909\u092A\u092F\u094B\u0917\u2026"
title: "\u0921\u0940\u092C\u0917 \u0906\u0909\u091F\u092A\u0941\u091F \u092A\u094D\
  \u0930\u093F\u0902\u091F \u0915\u0930\u0928\u093E"
weight: 33
---

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
