---
date: 2024-01-20 17:35:03.182087-07:00
description: "\u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917 \u0915\u0949\u0928\u094D\
  \u0915\u0947\u091F\u093F\u0928\u0947\u0936\u0928 \u0938\u0947 \u0906\u0936\u092F\
  \ \u0939\u0948 \u0926\u094B \u092F\u093E \u0926\u094B \u0938\u0947 \u091C\u094D\u092F\
  \u093E\u0926\u093E \u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917\u094D\u0938\
  \ \u0915\u094B \u091C\u094B\u0921\u093C\u0928\u093E\u0964 \u092A\u094D\u0930\u094B\
  \u0917\u094D\u0930\u093E\u092E\u0930\u094D\u0938 \u0906\u092E\u0924\u094C\u0930\
  \ \u092A\u0930 \u0907\u0938\u0915\u093E \u0907\u0938\u094D\u0924\u0947\u092E\u093E\
  \u0932 \u092F\u0942\u091C\u093C\u0930 \u0907\u0902\u091F\u0930\u092B\u0947\u0938\
  \ \u092E\u0948\u0938\u0947\u091C\u0947\u0938, \u0932\u0949\u0917\u094D\u0938 \u0914\
  \u0930 \u0915\u0949\u092E\u094D\u092A\u094D\u0932\u0947\u0915\u094D\u0938\u2026"
lastmod: '2024-03-13T22:44:52.099784-06:00'
model: gpt-4-1106-preview
summary: "\u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917 \u0915\u0949\u0928\u094D\
  \u0915\u0947\u091F\u093F\u0928\u0947\u0936\u0928 \u0938\u0947 \u0906\u0936\u092F\
  \ \u0939\u0948 \u0926\u094B \u092F\u093E \u0926\u094B \u0938\u0947 \u091C\u094D\u092F\
  \u093E\u0926\u093E \u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917\u094D\u0938\
  \ \u0915\u094B \u091C\u094B\u0921\u093C\u0928\u093E\u0964 \u092A\u094D\u0930\u094B\
  \u0917\u094D\u0930\u093E\u092E\u0930\u094D\u0938 \u0906\u092E\u0924\u094C\u0930\
  \ \u092A\u0930 \u0907\u0938\u0915\u093E \u0907\u0938\u094D\u0924\u0947\u092E\u093E\
  \u0932 \u092F\u0942\u091C\u093C\u0930 \u0907\u0902\u091F\u0930\u092B\u0947\u0938\
  \ \u092E\u0948\u0938\u0947\u091C\u0947\u0938, \u0932\u0949\u0917\u094D\u0938 \u0914\
  \u0930 \u0915\u0949\u092E\u094D\u092A\u094D\u0932\u0947\u0915\u094D\u0938 \u0921\
  \u093E\u091F\u093E \u0915\u0947 \u092A\u094D\u0930\u0926\u0930\u094D\u0936\u0928\
  \ \u0915\u0947 \u0932\u093F\u090F \u0915\u0930\u0924\u0947 \u0939\u0948\u0902\u0964\
  ."
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
