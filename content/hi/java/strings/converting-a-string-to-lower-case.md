---
date: 2024-01-20 17:39:26.067226-07:00
description: "How to: (\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902:) \u091C\u093E\
  \u0935\u093E \u092E\u0947\u0902 `toLowerCase()` \u092E\u0947\u0925\u0921 \u0907\u0938\
  \u094D\u0924\u0947\u092E\u093E\u0932 \u0915\u093E \u092A\u0941\u0930\u093E\u0928\
  \u093E \u0914\u0930 \u092E\u093E\u0928\u0915 \u0924\u0930\u0940\u0915\u093E \u0939\
  \u0948\u0964 \u092F\u0939 \u092E\u0947\u0925\u0921 \u092F\u0942\u0928\u093F\u0915\
  \u094B\u0921 \u0938\u094D\u091F\u0948\u0902\u0921\u0930\u094D\u0921 \u0915\u093E\
  \ \u092A\u093E\u0932\u0928 \u0915\u0930\u0924\u093E \u0939\u0948, \u091C\u093F\u0938\
  \u0938\u0947 \u0905\u0932\u0917-\u0905\u0932\u0917 \u092D\u093E\u0937\u093E\u0913\
  \u0901 \u0915\u0947\u2026"
lastmod: '2024-04-05T22:51:06.775541-06:00'
model: gpt-4-1106-preview
summary: "(\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902:) \u091C\u093E\u0935\u093E\
  \ \u092E\u0947\u0902 `toLowerCase()` \u092E\u0947\u0925\u0921 \u0907\u0938\u094D\
  \u0924\u0947\u092E\u093E\u0932 \u0915\u093E \u092A\u0941\u0930\u093E\u0928\u093E\
  \ \u0914\u0930 \u092E\u093E\u0928\u0915 \u0924\u0930\u0940\u0915\u093E \u0939\u0948\
  \u0964 \u092F\u0939 \u092E\u0947\u0925\u0921 \u092F\u0942\u0928\u093F\u0915\u094B\
  \u0921 \u0938\u094D\u091F\u0948\u0902\u0921\u0930\u094D\u0921 \u0915\u093E \u092A\
  \u093E\u0932\u0928 \u0915\u0930\u0924\u093E \u0939\u0948, \u091C\u093F\u0938\u0938\
  \u0947 \u0905\u0932\u0917-\u0905\u0932\u0917 \u092D\u093E\u0937\u093E\u0913\u0901\
  \ \u0915\u0947 \u0905\u0915\u094D\u0937\u0930 \u0938\u0939\u0940 \u0938\u0947 \u091B\
  \u094B\u091F\u0947 \u0939\u094B\u0902\u0964 \u0905\u0932\u094D\u091F\u0930\u0928\
  \u0947\u091F\u093F\u0935\u094D\u0938 \u092E\u0947\u0902 `StringUtils.lowerCase()`\
  \ \u092D\u0940 \u0939\u0948 \u091C\u094B Apache Commons Lang \u0932\u093E\u0907\u092C\
  \u094D\u0930\u0947\u0930\u0940 \u092E\u0947\u0902 \u0939\u0948\u0964 \u0907\u0938\
  \u092E\u0947\u0902 \u0905\u0924\u093F\u0930\u093F\u0915\u094D\u0924 \u092B\u0902\
  \u0915\u094D\u0936\u0928\u0932\u093F\u091F\u0940 \u0939\u094B\u0924\u0940 \u0939\
  \u0948 \u091C\u0948\u0938\u0947 \u0928\u0932\u094D\u0932 (null) \u0938\u094D\u091F\
  \u094D\u0930\u093F\u0902\u0917 \u0939\u0948\u0902\u0921\u0932\u093F\u0902\u0917\u0964\
  \ \u090F\u0915 \u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917 \u0915\u094B \u0932\
  \u094B\u0905\u0930 \u0915\u0947\u0938 \u092E\u0947\u0902 \u092C\u0926\u0932\u0928\
  \u0947 \u0915\u0940 \u0907\u0902\u092A\u094D\u0932\u0940\u092E\u0947\u0902\u091F\
  \u0947\u0936\u0928 \u0915\u093E \u0935\u093F\u0935\u0930\u0923 \u092E\u0939\u0924\
  \u094D\u0935\u092A\u0942\u0930\u094D\u0923 \u0939\u0948\u0964 \u091C\u093E\u0935\
  \u093E \u0915\u093E `toLowerCase()` \u0932\u094B\u0915\u0947\u0932-\u0906\u0927\u093E\
  \u0930\u093F\u0924 \u0939\u094B \u0938\u0915\u0924\u093E \u0939\u0948\u0964 \u092F\
  \u0939 \u0905\u0928\u0941\u0915\u0942\u0932 \u0939\u0948 \u0905\u0917\u0930 \u0906\
  \u092A \u092E\u0932\u094D\u091F\u0940-\u0915\u0932\u094D\u091A\u0930\u0932 \u0910\
  \u092A\u094D\u092A \u0935\u093F\u0915\u0938\u093F\u0924 \u0915\u0930 \u0930\u0939\
  \u0947 \u0939\u094B\u0902\u0964 \u0939\u093E\u0932\u093E\u0902\u0915\u093F, \u0906\
  \u092A \u092C\u093F\u0928\u093E \u0932\u094B\u0915\u0947\u0932 \u0915\u0947 \u092D\
  \u0940 `toLowerCase()` \u0915\u093E \u0907\u0938\u094D\u0924\u0947\u092E\u093E\u0932\
  \ \u0915\u0930 \u0938\u0915\u0924\u0947 \u0939\u0948\u0902\u0964."
title: "\u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917 \u0915\u094B \u091B\u094B\
  \u091F\u0947 \u0905\u0915\u094D\u0937\u0930\u094B\u0902 \u092E\u0947\u0902 \u092A\
  \u0930\u093F\u0935\u0930\u094D\u0924\u093F\u0924 \u0915\u0930\u0928\u093E"
weight: 4
---

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
