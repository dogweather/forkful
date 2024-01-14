---
title:                "Arduino: पैटर्न से मेल खाने वाले अक्षरों को हटाना"
programming_language: "Arduino"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/arduino/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## क्यों

क्या आप कभी अपने Arduino प्रोग्राम में से कुछ विशेष पैटर्न के साथ कारूनों को हटाने की आवश्यकता महसूस करते हैं? यह इसलिए हो सकता है कि आपके कोड में कुछ अधिकतम पैटर्न होते हैं जो बेकार हो गए हैं और आप चाहते हैं कि उन्हें हटाकर कोड को साफ़ करें। इस ब्लॉग पोस्ट में, हम आपको बताएँगे कि आप अपने Arduino कोड में से कैसे पैटर्न मैचिंग के साथ कारूनों को हटा सकते हैं। तो चलिए शुरू करते हैं!

## कैसे करें

```arduino
String inputString = "Hello World!";
String pattern = "o";

while (inputString.indexOf(pattern) >= 0) {
  int index = inputString.indexOf(pattern);
  inputString.remove(index, 1);
}

Serial.println(inputString); // Output: Hell Wrld!
```

इस कोड में हमने `inputString` में से `pattern` को हटाया है जो हमारे कोड में से स्ट्रिंग `o` के साथ मैच करता है। हम `while` लूप का उपयोग करते हैं ताकि हम स्ट्रिंग में से सभी समान पैटर्न को हटा सकें। यहां हम `inputString.indexOf()` फ़ंक्शन का उपयोग कर रहे हैं जो स्ट्रिंग में से पैटर्न का पहला इंडेक्स लौटाता है। तो हमें उस इंडेक्स को उपयोग करके स्ट्रिंग से पैटर्न को हटाना है। अंत में, हम उपयोग करते हैं `Serial.println()` फ़ंक्शन को उस वर्तमान स्ट्रिंग को प्रिंट करने के लिए।

## गहराई में जाएं

इस कोड में हमने स्ट्रिंग से पैटर्न को हटाने के लिए `remove()` फ़ंक्शन का उपयोग किया है। इस फ़ंक्शन को दो तरह से उपयोग किया जा सकता है - एक इंडेक्स के साथ और एक शु