---
title:                "पैटर्न से मेल खाते अक्षरों को हटाना"
aliases:
- hi/java/deleting-characters-matching-a-pattern.md
date:                  2024-01-20T17:43:04.947819-07:00
model:                 gpt-4-1106-preview
simple_title:         "पैटर्न से मेल खाते अक्षरों को हटाना"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/java/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
पैटर्न मैचिंग वाले कैरेक्टर्स को डिलीट करना मतलब है किसी स्ट्रिंग से खास तरह के कैरेक्टर्स को हटाना जो एक पैटर्न को मैच करते हों। प्रोग्रामर्स इसे डेटा साफ करने, इनपुट वेलिडेशन, और जरूरत के अनुसार टेक्स्ट को फॉर्मेट करने के लिए करते हैं।

## How to: (कैसे करें:)
```java
import java.util.regex.Pattern;

public class PatternMatchDelete {
    public static void main(String[] args) {
        String inputString = "Hello, यह 123 एक Test स्ट्रिंग है!";
        String patternString = "[^\\p{L}\\p{Nd}]+";
        
        // कैरेक्टर्स डिलीट करने के लिए पैटर्न कंपाइल करना
        Pattern pattern = Pattern.compile(patternString);
        
        // पैटर्न मैचिंग वाले कैरेक्टर्स हटाना
        String cleanedString = pattern.matcher(inputString).replaceAll("");
        
        System.out.println("Original String: " + inputString);
        System.out.println("Cleaned String: " + cleanedString);
    }
}
```
सैंपल आउटपुट:
```
Original String: Hello, यह 123 एक Test स्ट्रिंग है!
Cleaned String: Helloयह123एकTestस्ट्रिंगहै
```

## Deep Dive (गहराई से जानकारी)
पैटर्न मैचिंग में अक्सर `Regular Expressions (RegEx)` का इस्तेमाल होता है। Java में `java.util.regex` पैकेज रेगेक्स फंक्शनलिटी को प्रोवाइड करता है। यह 1.4 में लाया गया था, और तब से डेटा वेलिडेशन और मैनिपुलेशन मैथड में इस्तेमाल होता आ रहा है। एल्टरनेटिव के रूप में आप थर्ड-पार्टी लाइब्रेरीज का उपयोग कर सकते हैं जैसे `Apache Commons Lang` जो `StringUtils` में इसी तरह के मेथड्स प्रोवाइड करती है, लेकिन `java.util.regex` स्टैंडर्ड और पावरफुल रहता है। जब कैरेक्टर्स को हटाने की बात आती है, तब परफॉरमेंस भी एक अहम फैक्टर होता है इसलिए पैटर्न को कॉम्पाइल कर कैशिंग करना और उसे रीयूज़ करने से परफॉरमेंस में सुधार होता है।

## See Also (और भी देखें)
- [Java Regex Tutorial](https://docs.oracle.com/javase/tutorial/essential/regex/)
- [Apache Commons StringUtils](https://commons.apache.org/proper/commons-lang/javadocs/api-release/org/apache/commons/lang3/StringUtils.html)
- [Effective Java by Joshua Bloch](http://www.informit.com/store/effective-java-9780134685991) - पुस्तक जो बताती है बेहतर कोडिंग प्रैक्टिसेज के बारे में
- [Java Performance: The Definitive Guide by Scott Oaks](http://shop.oreilly.com/product/0636920028499.do) - जावा का परफॉरमेंस सुधारने पर एक गाइड
