---
date: 2024-01-20 17:56:46.662451-07:00
description: "Command line arguments \u0935\u094B \u092A\u0948\u0930\u093E\u092E\u0940\
  \u091F\u0930 \u0939\u094B\u0924\u0947 \u0939\u0948\u0902 \u091C\u094B \u0939\u092E\
  \ Java \u092A\u094D\u0930\u094B\u0917\u094D\u0930\u093E\u092E \u0915\u094B \u0930\
  \u0928 \u0915\u0930\u0924\u0947 \u0935\u0915\u094D\u0924 pass \u0915\u0930\u0924\
  \u0947 \u0939\u0948\u0902\u0964 \u092F\u0947 \u0907\u0938\u0932\u093F\u090F \u091C\
  \u093C\u0930\u0942\u0930\u0940 \u0939\u094B\u0924\u0947 \u0939\u0948\u0902 \u0915\
  \u094D\u092F\u094B\u0902\u0915\u093F \u0907\u0928\u0915\u0940 \u092E\u0926\u0926\
  \ \u0938\u0947 \u0939\u092E program\u2026"
lastmod: '2024-03-11T00:14:26.038023-06:00'
model: gpt-4-1106-preview
summary: "Command line arguments \u0935\u094B \u092A\u0948\u0930\u093E\u092E\u0940\
  \u091F\u0930 \u0939\u094B\u0924\u0947 \u0939\u0948\u0902 \u091C\u094B \u0939\u092E\
  \ Java \u092A\u094D\u0930\u094B\u0917\u094D\u0930\u093E\u092E \u0915\u094B \u0930\
  \u0928 \u0915\u0930\u0924\u0947 \u0935\u0915\u094D\u0924 pass \u0915\u0930\u0924\
  \u0947 \u0939\u0948\u0902\u0964 \u092F\u0947 \u0907\u0938\u0932\u093F\u090F \u091C\
  \u093C\u0930\u0942\u0930\u0940 \u0939\u094B\u0924\u0947 \u0939\u0948\u0902 \u0915\
  \u094D\u092F\u094B\u0902\u0915\u093F \u0907\u0928\u0915\u0940 \u092E\u0926\u0926\
  \ \u0938\u0947 \u0939\u092E program\u2026"
title: "\u0915\u092E\u093E\u0902\u0921 \u0932\u093E\u0907\u0928 \u0906\u0930\u094D\
  \u0917\u0941\u092E\u0947\u0902\u091F\u094D\u0938 \u092A\u0922\u093C\u0928\u093E"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)

Command line arguments वो पैरामीटर होते हैं जो हम Java प्रोग्राम को रन करते वक्त pass करते हैं। ये इसलिए ज़रूरी होते हैं क्योंकि इनकी मदद से हम program behavior को फ्लेक्सिबली कंट्रोल कर पाते हैं।

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
