---
date: 2024-01-20 17:56:46.662451-07:00
description: "How to: (\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902:) \u091C\u092C\
  \ \u0939\u092E Java \u092E\u0947\u0902 \u092E\u0947\u0928 \u092E\u0947\u0925\u0921\
  \ \u0932\u093F\u0916\u0924\u0947 \u0939\u0948\u0902, `String[] args` \u0909\u0938\
  \u0915\u093E \u092A\u093E\u0930\u094D\u091F \u0939\u094B\u0924\u093E \u0939\u0948\
  \u0964 \u092F\u0947 `args` array \u092E\u0947\u0902 command line arguments \u0938\
  \u094D\u091F\u094B\u0930 \u0939\u094B\u0924\u0947 \u0939\u0948\u0902\u0964\u2026"
lastmod: '2024-04-05T21:53:54.150225-06:00'
model: gpt-4-1106-preview
summary: "(\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902:) \u091C\u092C \u0939\
  \u092E Java \u092E\u0947\u0902 \u092E\u0947\u0928 \u092E\u0947\u0925\u0921 \u0932\
  \u093F\u0916\u0924\u0947 \u0939\u0948\u0902, `String[] args` \u0909\u0938\u0915\u093E\
  \ \u092A\u093E\u0930\u094D\u091F \u0939\u094B\u0924\u093E \u0939\u0948\u0964 \u092F\
  \u0947 `args` array \u092E\u0947\u0902 command line arguments \u0938\u094D\u091F\
  \u094B\u0930 \u0939\u094B\u0924\u0947 \u0939\u0948\u0902\u0964 \u0928\u0940\u091A\
  \u0947 \u0926\u0947\u0916\u094B \u0915\u0948\u0938\u0947."
title: "\u0915\u092E\u093E\u0902\u0921 \u0932\u093E\u0907\u0928 \u0906\u0930\u094D\
  \u0917\u0941\u092E\u0947\u0902\u091F\u094D\u0938 \u092A\u0922\u093C\u0928\u093E"
weight: 23
---

## How to: (कैसे करें:)
जब हम Java में मेन मेथड लिखते हैं, `String[] args` उसका पार्ट होता है। ये `args` array में command line arguments स्टोर होते हैं। नीचे देखो कैसे:

```java
public class CommandLineExample {
    public static void main(String[] args) {
        if (args.length > 0) { 
            for (String arg : args) {
                System.out.println("Argument: " + arg);
            }
        } else {
            System.out.println("No command line arguments found.");
        }
    }
}
```

अगर आप इस कोड को रन करो और arguments दो, तो output कुछ ऐसा होगा:

```
Argument: Hello
Argument: World
```

## Deep Dive (गहराई में जानकारी):
Command line arguments इस्तेमाल करने की प्रक्रिया पुरानी है, और ये लगभग सभी programming languages में मौजूद है। इनका प्रयोग करके हम settings या फाइल के paths को dynamically specify कर सकते हैं। यह flexibility प्रोग्राम को बहुत powerful बनाती है।

अलग methods भी हैं command line से डेटा प्राप्त करने के लिए, जैसे कि `Scanner` class या Java NIO पैकेज।

लेकिन जब एप्लिकेशन को कस्टमाइज़ करना हो या किसी script के साथ इंटरफेस करना हो, command line arguments का प्रयोग बहुत आसान और एफेक्टिव होता है। 

विशेष implementation की बात करें तो, Java `main` मेथड का `String[] args` पैरामीटर हमेशा non-null होता है। अगर कोई arguments पास नहीं किया गया, तो भी यह एक खाली `String` array होता है।

## See Also (और भी देखें):
- [Oracle Java Documentation on Command-Line Arguments](https://docs.oracle.com/javase/tutorial/essential/environment/cmdLineArgs.html)
- [Java 11 Docs](https://docs.oracle.com/en/java/javase/11/)
- [Stack Overflow Discussion on Java Arguments](https://stackoverflow.com/questions/890966/what-are-command-line-arguments-in-java)
