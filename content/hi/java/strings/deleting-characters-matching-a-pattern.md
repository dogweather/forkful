---
aliases:
- /hi/java/deleting-characters-matching-a-pattern/
date: 2024-01-20 17:43:04.947819-07:00
description: "\u092A\u0948\u091F\u0930\u094D\u0928 \u092E\u0948\u091A\u093F\u0902\u0917\
  \ \u0935\u093E\u0932\u0947 \u0915\u0948\u0930\u0947\u0915\u094D\u091F\u0930\u094D\
  \u0938 \u0915\u094B \u0921\u093F\u0932\u0940\u091F \u0915\u0930\u0928\u093E \u092E\
  \u0924\u0932\u092C \u0939\u0948 \u0915\u093F\u0938\u0940 \u0938\u094D\u091F\u094D\
  \u0930\u093F\u0902\u0917 \u0938\u0947 \u0916\u093E\u0938 \u0924\u0930\u0939 \u0915\
  \u0947 \u0915\u0948\u0930\u0947\u0915\u094D\u091F\u0930\u094D\u0938 \u0915\u094B\
  \ \u0939\u091F\u093E\u0928\u093E \u091C\u094B \u090F\u0915 \u092A\u0948\u091F\u0930\
  \u094D\u0928 \u0915\u094B \u092E\u0948\u091A \u0915\u0930\u0924\u0947 \u0939\u094B\
  \u0902\u0964 \u092A\u094D\u0930\u094B\u0917\u094D\u0930\u093E\u092E\u0930\u094D\u0938\
  \ \u0907\u0938\u0947 \u0921\u0947\u091F\u093E\u2026"
lastmod: 2024-02-18 23:09:03.085794
model: gpt-4-1106-preview
summary: "\u092A\u0948\u091F\u0930\u094D\u0928 \u092E\u0948\u091A\u093F\u0902\u0917\
  \ \u0935\u093E\u0932\u0947 \u0915\u0948\u0930\u0947\u0915\u094D\u091F\u0930\u094D\
  \u0938 \u0915\u094B \u0921\u093F\u0932\u0940\u091F \u0915\u0930\u0928\u093E \u092E\
  \u0924\u0932\u092C \u0939\u0948 \u0915\u093F\u0938\u0940 \u0938\u094D\u091F\u094D\
  \u0930\u093F\u0902\u0917 \u0938\u0947 \u0916\u093E\u0938 \u0924\u0930\u0939 \u0915\
  \u0947 \u0915\u0948\u0930\u0947\u0915\u094D\u091F\u0930\u094D\u0938 \u0915\u094B\
  \ \u0939\u091F\u093E\u0928\u093E \u091C\u094B \u090F\u0915 \u092A\u0948\u091F\u0930\
  \u094D\u0928 \u0915\u094B \u092E\u0948\u091A \u0915\u0930\u0924\u0947 \u0939\u094B\
  \u0902\u0964 \u092A\u094D\u0930\u094B\u0917\u094D\u0930\u093E\u092E\u0930\u094D\u0938\
  \ \u0907\u0938\u0947 \u0921\u0947\u091F\u093E\u2026"
title: "\u092A\u0948\u091F\u0930\u094D\u0928 \u0938\u0947 \u092E\u0947\u0932 \u0916\
  \u093E\u0924\u0947 \u0905\u0915\u094D\u0937\u0930\u094B\u0902 \u0915\u094B \u0939\
  \u091F\u093E\u0928\u093E"
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
