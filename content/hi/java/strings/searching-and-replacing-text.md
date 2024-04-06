---
date: 2024-01-20 17:58:37.450433-07:00
description: "How to? (\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902?) \u091C\u093E\
  \u0935\u093E \u092E\u0947\u0902 \u091F\u0947\u0915\u094D\u0938\u094D\u091F \u0938\
  \u0930\u094D\u091A \u0914\u0930 \u0930\u093F\u092A\u094D\u0932\u0947\u0938 \u092B\
  \u0902\u0915\u094D\u0936\u0928\u094D\u0938 \u0915\u0940 \u0936\u0941\u0930\u0941\
  \u0906\u0924 J2SE 1.4 \u092E\u0947\u0902 `replaceAll` \u0914\u0930 `replaceFirst`\
  \ \u092E\u0947\u0925\u0921\u094D\u0938 \u0915\u0947 \u091C\u0930\u093F\u090F \u0939\
  \u0941\u0908 \u0925\u0940, \u091C\u094B\u2026"
lastmod: '2024-04-05T22:51:06.772822-06:00'
model: gpt-4-1106-preview
summary: "How to? (\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902?) \u091C\u093E\
  \u0935\u093E \u092E\u0947\u0902 \u091F\u0947\u0915\u094D\u0938\u094D\u091F \u0938\
  \u0930\u094D\u091A \u0914\u0930 \u0930\u093F\u092A\u094D\u0932\u0947\u0938 \u092B\
  \u0902\u0915\u094D\u0936\u0928\u094D\u0938 \u0915\u0940 \u0936\u0941\u0930\u0941\
  \u0906\u0924 J2SE 1.4 \u092E\u0947\u0902 `replaceAll` \u0914\u0930 `replaceFirst`\
  \ \u092E\u0947\u0925\u0921\u094D\u0938 \u0915\u0947 \u091C\u0930\u093F\u090F \u0939\
  \u0941\u0908 \u0925\u0940, \u091C\u094B `java.lang.String` \u0915\u094D\u0932\u093E\
  \u0938 \u0915\u093E \u0939\u093F\u0938\u094D\u0938\u093E \u0939\u0948\u0902\u0964\
  \ \u0906\u091C \u092D\u0940, \u092F\u0947 \u092B\u0902\u0915\u094D\u0936\u0928\u094D\
  \u0938 `Pattern` \u0914\u0930 `Matcher` \u091C\u0948\u0938\u0947 \u0930\u0947\u0917\
  \u0941\u0932\u0930 \u090F\u0915\u094D\u0938\u092A\u094D\u0930\u0947\u0936\u0928\
  \ \u0915\u094D\u0932\u093E\u0938\u0947\u091C \u0915\u0947 \u0938\u093E\u0925 \u0915\
  \u093E\u092E \u0915\u0930\u0924\u0947 \u0939\u0948\u0902\u0964 \u092F\u0939 \u0928\
  \ \u0915\u0947\u0935\u0932 \u092C\u0947\u0938\u093F\u0915 \u0938\u094D\u091F\u094D\
  \u0930\u093F\u0902\u0917\u094D\u0938 \u0915\u0947 \u0932\u093F\u090F \u092C\u0932\
  \u094D\u0915\u093F \u091C\u091F\u093F\u0932 \u091F\u0947\u0915\u094D\u0938\u094D\
  \u091F \u092A\u0948\u091F\u0930\u094D\u0928\u094D\u0938 \u0915\u0947 \u0932\u093F\
  \u090F \u092D\u0940 \u0909\u092A\u092F\u094B\u0917\u0940 \u0939\u0948\u0902\u0964\
  ."
title: "\u092A\u093E\u0920 \u0916\u094B\u091C\u0928\u093E \u0914\u0930 \u092C\u0926\
  \u0932\u0928\u093E"
weight: 10
---

## How to? (कैसे करें?)
```java
public class TextSearchReplace {
    public static void main(String[] args) {
        String originalText = "फूल बहुत सुंदर होते हैं।";
        String searchText = "फूल";
        String replaceText = "तारे";

        String replacedText = originalText.replace(searchText, replaceText);

        System.out.println("पहले: " + originalText);
        System.out.println("बाद में: " + replacedText);
    }
}
```
सैंपल आउटपुट:
```
पहले: फूल बहुत सुंदर होते हैं।
बाद में: तारे बहुत सुंदर होते हैं।
```

## Deep Dive (गहराई से जानकारी)
जावा में टेक्स्ट सर्च और रिप्लेस फंक्शन्स की शुरुआत J2SE 1.4 में `replaceAll` और `replaceFirst` मेथड्स के जरिए हुई थी, जो `java.lang.String` क्लास का हिस्सा हैं। आज भी, ये फंक्शन्स `Pattern` और `Matcher` जैसे रेगुलर एक्सप्रेशन क्लासेज के साथ काम करते हैं। यह न केवल बेसिक स्ट्रिंग्स के लिए बल्कि जटिल टेक्स्ट पैटर्न्स के लिए भी उपयोगी हैं।

```java
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class RegexExample {
    public static void main(String[] args) {
        String originalText = "बगीचे में 2 फूल और 5 फूल!";
        Pattern pattern = Pattern.compile("\\bफूल\\b");
        Matcher matcher = pattern.matcher(originalText);

        String replacedText = matcher.replaceAll("तारे");
        System.out.println(replacedText);
    }
}
```

ध्यान दें, रेगुलर एक्सप्रेशन में `\\b` का मतलब है 'word boundary' जिससे सिर्फ अकेला शब्द 'फूल' ही बदलेगा।

## See Also (और भी जानकारी)
- Java Documentation on String Class: https://docs.oracle.com/javase/7/docs/api/java/lang/String.html
- Java Documentation on Pattern Class: https://docs.oracle.com/javase/7/docs/api/java/util/regex/Pattern.html
- Java Documentation on Matcher Class: https://docs.oracle.com/javase/7/docs/api/java/util/regex/Matcher.html
- Online regex tester and debugger: https://regex101.com/
