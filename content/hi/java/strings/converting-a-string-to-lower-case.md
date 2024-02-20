---
date: 2024-01-20 17:39:26.067226-07:00
description: "\u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917 \u0915\u094B \u0932\
  \u094B\u0905\u0930 \u0915\u0947\u0938 \u092E\u0947\u0902 \u092C\u0926\u0932\u0928\
  \u0947 \u0915\u093E \u092E\u0924\u0932\u092C \u0939\u0948 \u0938\u092D\u0940 \u0905\
  \u0915\u094D\u0937\u0930\u094B\u0902 \u0915\u094B \u091B\u094B\u091F\u093E (lower\
  \ case) \u0915\u0930\u0928\u093E\u0964 \u092A\u094D\u0930\u094B\u0917\u094D\u0930\
  \u093E\u092E\u0930\u094D\u0938 \u0907\u0938\u0915\u094B \u0905\u0915\u094D\u0938\
  \u0930 \u0915\u0930\u0924\u0947 \u0939\u0948\u0902 \u0924\u093E\u0915\u093F \u0915\
  \u0902\u092A\u0947\u092F\u0930\u093F\u091C\u0928\u094D\u0938 (comparisons), \u0938\
  \u0930\u094D\u091A \u0914\u0930\u2026"
lastmod: 2024-02-19 22:05:11.087745
model: gpt-4-1106-preview
summary: "\u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917 \u0915\u094B \u0932\u094B\
  \u0905\u0930 \u0915\u0947\u0938 \u092E\u0947\u0902 \u092C\u0926\u0932\u0928\u0947\
  \ \u0915\u093E \u092E\u0924\u0932\u092C \u0939\u0948 \u0938\u092D\u0940 \u0905\u0915\
  \u094D\u0937\u0930\u094B\u0902 \u0915\u094B \u091B\u094B\u091F\u093E (lower case)\
  \ \u0915\u0930\u0928\u093E\u0964 \u092A\u094D\u0930\u094B\u0917\u094D\u0930\u093E\
  \u092E\u0930\u094D\u0938 \u0907\u0938\u0915\u094B \u0905\u0915\u094D\u0938\u0930\
  \ \u0915\u0930\u0924\u0947 \u0939\u0948\u0902 \u0924\u093E\u0915\u093F \u0915\u0902\
  \u092A\u0947\u092F\u0930\u093F\u091C\u0928\u094D\u0938 (comparisons), \u0938\u0930\
  \u094D\u091A \u0914\u0930\u2026"
title: "\u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917 \u0915\u094B \u091B\u094B\
  \u091F\u0947 \u0905\u0915\u094D\u0937\u0930\u094B\u0902 \u092E\u0947\u0902 \u092A\
  \u0930\u093F\u0935\u0930\u094D\u0924\u093F\u0924 \u0915\u0930\u0928\u093E"
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
