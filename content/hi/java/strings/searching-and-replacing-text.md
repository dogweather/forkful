---
title:                "पाठ खोजना और बदलना"
aliases: - /hi/java/searching-and-replacing-text.md
date:                  2024-01-20T17:58:37.450433-07:00
model:                 gpt-4-1106-preview
simple_title:         "पाठ खोजना और बदलना"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/java/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
टेक्स्ट सर्च और रिप्लेस का मतलब है मौजूदा टेक्स्ट में कुछ शब्दों को ढूंढना और फिर उन्हें दूसरे शब्दों से बदल देना। प्रोग्रामर्स इसे डेटा को संशोधित करने, बग्स ठीक करने या कोड को अपडेट करने के लिए करते हैं।

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
