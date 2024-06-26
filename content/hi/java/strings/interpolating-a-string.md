---
date: 2024-01-20 17:51:50.582592-07:00
description: "How to: (\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902:) \u092A\u0941\
  \u0930\u093E\u0928\u0947 \u091C\u093E\u0935\u093E \u0935\u0930\u094D\u0936\u0902\
  \u0938 \u092E\u0947\u0902 '+' \u0911\u092A\u0930\u0947\u091F\u0930 \u0938\u094D\u091F\
  \u094D\u0930\u093F\u0902\u0917\u094D\u0938 \u0915\u094B \u091C\u094B\u0921\u093C\
  \u0928\u0947 \u0915\u0947 \u0932\u093F\u090F \u0939\u0940 \u092E\u0941\u0916\u094D\
  \u092F \u0924\u0930\u0940\u0915\u093E \u0925\u093E. `String.format()` \u0914\u0930\
  \ `Formatter` \u0915\u094D\u0932\u093E\u0938 \u0928\u0947 \u092B\u0949\u0930\u094D\
  \u092E\u0947\u091F\u0947\u0921\u2026"
lastmod: '2024-04-05T22:51:06.774250-06:00'
model: gpt-4-1106-preview
summary: "(\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902:) \u092A\u0941\u0930\u093E\
  \u0928\u0947 \u091C\u093E\u0935\u093E \u0935\u0930\u094D\u0936\u0902\u0938 \u092E\
  \u0947\u0902 '+' \u0911\u092A\u0930\u0947\u091F\u0930 \u0938\u094D\u091F\u094D\u0930\
  \u093F\u0902\u0917\u094D\u0938 \u0915\u094B \u091C\u094B\u0921\u093C\u0928\u0947\
  \ \u0915\u0947 \u0932\u093F\u090F \u0939\u0940 \u092E\u0941\u0916\u094D\u092F \u0924\
  \u0930\u0940\u0915\u093E \u0925\u093E."
title: "\u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917 \u0907\u0902\u091F\u0930\u092A\
  \u094B\u0932\u0947\u0936\u0928"
weight: 8
---

## How to: (कैसे करें:)
```Java
public class StringInterpolationExample {
    public static void main(String[] args) {
        String name = "विजय";
        int age = 25;

        // Java 15 से पहले: "+" के इस्तेमाल से 
        String message = "नमस्ते, मेरा नाम है " + name + " और मैं " + age + " वर्ष का हूँ।";
        System.out.println(message);

        // Java 15 और उसके बाद: String.format का इस्तेमाल करते हुए
        String newMessage = String.format("नमस्ते, मेरा नाम है %s और मैं %d वर्ष का हूँ।", name, age);
        System.out.println(newMessage);
        
        // Java 15: Text Blocks का इस्तेमाल करते हुए (मल्टी-लाइन स्ट्रिंग्स)
        String bigMessage = """
                नमस्ते,
                मेरा नाम है %s
                और मैं %d वर्ष का हूँ।
                """.formatted(name, age);
        System.out.println(bigMessage);
    }
}
```
सैंपल आउटपुट:
```
नमस्ते, मेरा नाम है विजय और मैं 25 वर्ष का हूँ।
नमस्ते, मेरा नाम है विजय और मैं 25 वर्ष का हूँ।
नमस्ते,
मेरा नाम है विजय
और मैं 25 वर्ष का हूँ।
```

## Deep Dive (गहराई से जानकारी)
पुराने जावा वर्शंस में '+' ऑपरेटर स्ट्रिंग्स को जोड़ने के लिए ही मुख्य तरीका था. `String.format()` और `Formatter` क्लास ने फॉर्मेटेड स्ट्रिंग्स बनाने में सुधार किया. Java 15 से, टेक्स्ट ब्लॉक्स और `.formatted()` मेथड ने टेम्प्लेट्स को और भी स्पष्ट और संक्षिप्त बना दिया है.

अगर एकाधिक स्ट्रिंग्स को जोड़ना है, तो `StringBuilder` या `StringBuffer` का इस्तेमाल करना चाहिए, यह एफिशिएंसी के लिहाज से बेहतर है. स्ट्रिंग इंटरपोलेशन सीधे जावा में नहीं है, लेकिन उपर्युक्त तकनीकियाँ इसकी जरूरतों को पूरा करती हैं.

## See Also (अधिक जानकारी)
- [Oracle's official Java documentation on String.format](https://docs.oracle.com/en/java/javase/15/docs/api/java.base/java/lang/String.html#format(java.lang.String,java.lang.Object...))
- [Text Blocks (JEP 378)](https://openjdk.java.net/jeps/378)
