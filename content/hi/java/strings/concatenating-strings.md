---
date: 2024-01-20 17:35:03.182087-07:00
description: "How to: (\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902:) \u0938\u094D\
  \u091F\u094D\u0930\u093F\u0902\u0917 \u0915\u0949\u0928\u094D\u0915\u0947\u091F\u093F\
  \u0928\u0947\u0936\u0928 \u0915\u093E \u0907\u0938\u094D\u0924\u0947\u092E\u093E\
  \u0932 \u092A\u0941\u0930\u093E\u0928\u0947 \u0926\u093F\u0928\u094B\u0902 \u0938\
  \u0947 \u0939\u094B \u0930\u0939\u093E \u0939\u0948, \u091C\u092C \u0938\u0947 \u0938\
  \u094D\u091F\u094D\u0930\u093F\u0902\u0917\u094D\u0938 \u0915\u093E \u0921\u093E\
  \u091F\u093E \u091F\u093E\u0907\u092A \u0906\u092F\u093E\u0964 Java \u092E\u0947\
  \u0902, `+` \u0911\u092A\u0930\u0947\u091F\u0930 \u0938\u092C\u0938\u0947 \u0938\
  \u093E\u0927\u093E\u0930\u0923\u2026"
lastmod: '2024-04-05T22:51:06.784260-06:00'
model: gpt-4-1106-preview
summary: "(\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902:) \u0938\u094D\u091F\u094D\
  \u0930\u093F\u0902\u0917 \u0915\u0949\u0928\u094D\u0915\u0947\u091F\u093F\u0928\u0947\
  \u0936\u0928 \u0915\u093E \u0907\u0938\u094D\u0924\u0947\u092E\u093E\u0932 \u092A\
  \u0941\u0930\u093E\u0928\u0947 \u0926\u093F\u0928\u094B\u0902 \u0938\u0947 \u0939\
  \u094B \u0930\u0939\u093E \u0939\u0948, \u091C\u092C \u0938\u0947 \u0938\u094D\u091F\
  \u094D\u0930\u093F\u0902\u0917\u094D\u0938 \u0915\u093E \u0921\u093E\u091F\u093E\
  \ \u091F\u093E\u0907\u092A \u0906\u092F\u093E\u0964 Java \u092E\u0947\u0902, `+`\
  \ \u0911\u092A\u0930\u0947\u091F\u0930 \u0938\u092C\u0938\u0947 \u0938\u093E\u0927\
  \u093E\u0930\u0923 \u0924\u0930\u0940\u0915\u093E \u0939\u0948 \u0938\u094D\u091F\
  \u094D\u0930\u093F\u0902\u0917\u094D\u0938 \u091C\u094B\u0921\u093C\u0928\u0947\
  \ \u0915\u093E, \u092A\u0930 \u092F\u0939 \u091C\u094D\u092F\u093E\u0926\u093E \u0938\
  \u094D\u091F\u094D\u0930\u093F\u0902\u0917\u094D\u0938 \u0915\u0947 \u0938\u093E\
  \u0925 inefficient \u0939\u094B \u0938\u0915\u0924\u093E \u0939\u0948 \u0915\u094D\
  \u092F\u094B\u0902\u0915\u093F \u0939\u0930 `+` \u0915\u094D\u0930\u093F\u092F\u093E\
  \ \u090F\u0915 \u0928\u0908 \u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917 \u092C\
  \u0928\u093E\u0924\u093E \u0939\u0948\u0964 `StringBuilder` \u0915\u093E \u0909\u092A\
  \u092F\u094B\u0917 \u0915\u0930\u0928\u093E \u090F\u0915 \u092C\u0947\u0939\u0924\
  \u0930 \u0935\u093F\u0915\u0932\u094D\u092A \u0939\u0948, \u0959\u093E\u0938\u0915\
  \u0930 \u0932\u0942\u092A\u094D\u0938 \u0915\u0947 \u0905\u0902\u0926\u0930, \u0915\
  \u094D\u092F\u094B\u0902\u0915\u093F \u092F\u0939 \u092E\u0947\u092E\u094B\u0930\
  \u0940 \u0915\u093E \u092C\u0947\u0939\u0924\u0930 \u092A\u094D\u0930\u092C\u0902\
  \u0927\u0928 \u0915\u0930\u0924\u093E \u0939\u0948\u0964 `concat()` \u092E\u0947\
  \u0925\u0921 \u092D\u0940 \u0939\u0948, \u0932\u0947\u0915\u093F\u0928 \u092F\u0939\
  \ `+` \u0915\u0940 \u0924\u0941\u0932\u0928\u093E \u092E\u0947\u0902 \u0915\u092E\
  \ \u0907\u0938\u094D\u0924\u0947\u092E\u093E\u0932 \u092E\u0947\u0902 \u0932\u093E\
  \u092F\u093E \u091C\u093E\u0924\u093E \u0939\u0948\u0964."
title: "\u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917 \u0915\u094B \u091C\u094B\
  \u0921\u093C\u0928\u093E"
weight: 3
---

## How to: (कैसे करें:)
```java
public class StringConcatExample {
    public static void main(String[] args) {
        // Example 1: Using + operator
        String hello = "नमस्ते ";
        String world = "दुनिया!";
        String greeting = hello + world;
        System.out.println(greeting); // Output: नमस्ते दुनिया!

        // Example 2: Using concat() method
        String fullGreeting = hello.concat(world);
        System.out.println(fullGreeting); // Output: नमस्ते दुनिया!

        // Example 3: Using StringBuilder
        StringBuilder sb = new StringBuilder();
        sb.append(hello).append(world);
        System.out.println(sb.toString()); // Output: नमस्ते दुनिया!
    }
}
```

## Deep Dive (गहन जानकारी):
स्ट्रिंग कॉन्केटिनेशन का इस्तेमाल पुराने दिनों से हो रहा है, जब से स्ट्रिंग्स का डाटा टाइप आया। Java में, `+` ऑपरेटर सबसे साधारण तरीका है स्ट्रिंग्स जोड़ने का, पर यह ज्यादा स्ट्रिंग्स के साथ inefficient हो सकता है क्योंकि हर `+` क्रिया एक नई स्ट्रिंग बनाता है। `StringBuilder` का उपयोग करना एक बेहतर विकल्प है, ख़ासकर लूप्स के अंदर, क्योंकि यह मेमोरी का बेहतर प्रबंधन करता है। `concat()` मेथड भी है, लेकिन यह `+` की तुलना में कम इस्तेमाल में लाया जाता है।

## See Also (अन्य स्रोतों के लिंक):
- [Oracle Java Documentation for String](https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/lang/String.html)
- [Efficient String Concatenation in Java](https://www.baeldung.com/java-string-concatenation)
