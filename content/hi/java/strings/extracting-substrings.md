---
date: 2024-01-20 17:46:04.109850-07:00
description: "Java \u092E\u0947\u0902 substring \u0928\u093F\u0915\u093E\u0932\u0928\
  \u093E \u092E\u0924\u0932\u092C \u0939\u0948 \u0915\u093F\u0938\u0940 \u0938\u094D\
  \u091F\u094D\u0930\u093F\u0902\u0917 \u0915\u0947 \u090F\u0915 \u0939\u093F\u0938\
  \u094D\u0938\u0947 \u0915\u094B \u092A\u0915\u0921\u093C\u0928\u093E\u0964 \u0907\
  \u0938\u0947 \u092A\u094D\u0930\u094B\u0917\u094D\u0930\u093E\u092E\u0930\u094D\u0938\
  \ \u0924\u092C \u0907\u0938\u094D\u0924\u0947\u092E\u093E\u0932 \u0915\u0930\u0924\
  \u0947 \u0939\u0948\u0902 \u091C\u092C \u0909\u0928\u094D\u0939\u0947\u0902 \u0938\
  \u093F\u0930\u094D\u092B \u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917 \u0915\
  \u0947 \u090F\u0915 \u0916\u093E\u0938 \u0939\u093F\u0938\u094D\u0938\u0947\u2026"
lastmod: 2024-02-19 22:05:11.091151
model: gpt-4-1106-preview
summary: "Java \u092E\u0947\u0902 substring \u0928\u093F\u0915\u093E\u0932\u0928\u093E\
  \ \u092E\u0924\u0932\u092C \u0939\u0948 \u0915\u093F\u0938\u0940 \u0938\u094D\u091F\
  \u094D\u0930\u093F\u0902\u0917 \u0915\u0947 \u090F\u0915 \u0939\u093F\u0938\u094D\
  \u0938\u0947 \u0915\u094B \u092A\u0915\u0921\u093C\u0928\u093E\u0964 \u0907\u0938\
  \u0947 \u092A\u094D\u0930\u094B\u0917\u094D\u0930\u093E\u092E\u0930\u094D\u0938\
  \ \u0924\u092C \u0907\u0938\u094D\u0924\u0947\u092E\u093E\u0932 \u0915\u0930\u0924\
  \u0947 \u0939\u0948\u0902 \u091C\u092C \u0909\u0928\u094D\u0939\u0947\u0902 \u0938\
  \u093F\u0930\u094D\u092B \u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917 \u0915\
  \u0947 \u090F\u0915 \u0916\u093E\u0938 \u0939\u093F\u0938\u094D\u0938\u0947\u2026"
title: "\u0938\u092C\u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917\u094D\u0938 \u0928\
  \u093F\u0915\u093E\u0932\u0928\u093E"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)

Java में substring निकालना मतलब है किसी स्ट्रिंग के एक हिस्से को पकड़ना। इसे प्रोग्रामर्स तब इस्तेमाल करते हैं जब उन्हें सिर्फ स्ट्रिंग के एक खास हिस्से की जरूरत होती है, जैसे किसी यूजरनेम में से डोमेन निकालना या फिर प्रोडक्ट कोड में से खास सीरियल नंबर को पाना।

## How to: (कैसे करें:)

```java
public class SubstringExample {
    public static void main(String[] args) {
        String str = "HelloWorld";
        // पहले से चौथे करैक्टर तक का सबस्ट्रिंग
        String sub1 = str.substring(1, 5); // "ello"
        // छठे करैक्टर से आखिर तक का सबस्ट्रिंग
        String sub2 = str.substring(5); // "World"
        
        System.out.println("Substring 1: " + sub1);
        System.out.println("Substring 2: " + sub2);
    }
}
```

आउटपुट:
```
Substring 1: ello
Substring 2: World
```

## Deep Dive (गहराई से जानकारी):

`substring()` मेथड Java में बहुत पहले से है, और ये String class का एक महत्वपूर्ण हिस्सा रहा है। जैसे-जैसे Java अपग्रेड होता गया, `substring()` में भी सुधार होते रहे, जिससे ये और भी कुशल हो गया। अलग-अलग तरीकों में `StringBuffer` और `StringBuilder` जैसे क्लासेस का भी इस्तेमाल होता है, जैसे `substring()` का इस्तेमाल करके भी कर सकते हैं। इंप्लीमेंटेशन डिटेल्स की बात करें तो, पुराने Java वर्जन्स में `substring()` का इस्तेमाल थोड़ा संसाधन गहन होता था क्योंकि यह नए स्ट्रिंग के लिए हमेशा नई memory अलॉकेट करता था।

## See Also (इसे भी देखें):

- Oracle Java Documentation: https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/lang/String.html#substring(int,int)
- Java String tutorial from w3schools: https://www.w3schools.com/java/java_strings.asp
- Java String `substring()` method from GeeksforGeeks: https://www.geeksforgeeks.org/substring-in-java/
