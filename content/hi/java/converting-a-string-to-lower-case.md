---
title:                "स्ट्रिंग को छोटे अक्षरों में परिवर्तित करना"
date:                  2024-01-20T17:39:26.067226-07:00
model:                 gpt-4-1106-preview
simple_title:         "स्ट्रिंग को छोटे अक्षरों में परिवर्तित करना"

category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/java/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
स्ट्रिंग को लोअर केस में बदलने का मतलब है सभी अक्षरों को छोटा (lower case) करना। प्रोग्रामर्स इसको अक्सर करते हैं ताकि कंपेयरिजन्स (comparisons), सर्च और डेटा संसाधन सरल हो जाएं।

## How to: (कैसे करें:)
```java
public class LowerCaseExample {
    public static void main(String[] args) {
        String originalString = "नमस्ते, जावा वर्ल्ड!";
        String lowerCaseString = originalString.toLowerCase();
        
        System.out.println("Original: " + originalString);
        System.out.println("Lowercased: " + lowerCaseString);
    }
}
```
आउटपुट:
```
Original: नमस्ते, जावा वर्ल्ड!
Lowercased: नमस्ते, जावा वर्ल्ड!
```
ध्यान दें कि ऊपरी उदाहरण में, इंग्लिश अक्षरों के साथ-साथ हिंदी वर्णमाला के अक्षर भी लोअर केस में बदल गए हैं।

## Deep Dive (गहराई में जानकारी):
जावा में `toLowerCase()` मेथड इस्तेमाल का पुराना और मानक तरीका है। यह मेथड यूनिकोड स्टैंडर्ड का पालन करता है, जिससे अलग-अलग भाषाओँ के अक्षर सही से छोटे हों।

अल्टरनेटिव्स में `StringUtils.lowerCase()` भी है जो Apache Commons Lang लाइब्रेरी में है। इसमें अतिरिक्त फंक्शनलिटी होती है जैसे नल्ल (null) स्ट्रिंग हैंडलिंग।

एक स्ट्रिंग को लोअर केस में बदलने की इंप्लीमेंटेशन का विवरण महत्वपूर्ण है। जावा का `toLowerCase()` लोकेल-आधारित हो सकता है। यह अनुकूल है अगर आप मल्टी-कल्चरल ऐप्प विकसित कर रहे हों। हालांकि, आप बिना लोकेल के भी `toLowerCase()` का इस्तेमाल कर सकते हैं।

## See Also (इसे भी देखें):
- [Java String toLowerCase() Method](https://docs.oracle.com/en/java/javase/18/docs/api/java.base/java/lang/String.html#toLowerCase())
- [Unicode Standard](https://www.unicode.org/standard/standard.html)
- [Apache Commons Lang StringUtils](https://commons.apache.org/proper/commons-lang/apidocs/org/apache/commons/lang3/StringUtils.html#lowerCase(java.lang.CharSequence))
