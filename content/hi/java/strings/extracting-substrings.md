---
title:                "सबस्ट्रिंग्स निकालना"
aliases:
- /hi/java/extracting-substrings/
date:                  2024-01-20T17:46:04.109850-07:00
model:                 gpt-4-1106-preview
simple_title:         "सबस्ट्रिंग्स निकालना"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/java/extracting-substrings.md"
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
