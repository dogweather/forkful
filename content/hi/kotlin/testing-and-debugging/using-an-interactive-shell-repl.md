---
title:                "इंटरैक्टिव शेल (REPL) का उपयोग"
date:                  2024-01-26T04:16:27.396414-07:00
model:                 gpt-4-0125-preview
simple_title:         "इंटरैक्टिव शेल (REPL) का उपयोग"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/kotlin/using-an-interactive-shell-repl.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
REPL (Read-Eval-Print Loop) एक सरल, इंटरैक्टिव कंप्यूटर प्रोग्रामिंग वातावरण है। प्रोग्रामर इसका उपयोग त्वरित कोडिंग परीक्षण, स्निपेट्स की जाँच, या पूरी आवेदन बनाए बिना किसी भाषा के सिंटेक्स को सीखने के लिए करते हैं।

## कैसे:
Kotlin का REPL शुरू करना आसान है। अपना टर्मिनल खोलें और `kotlinc` टाइप करें। आप Kotlin शेल में पहुंच जाएंगे। आइए एक वैरिएबल परिभाषित करने और उसके मूल्य को प्रिंट करने की कोशिश करें:

```kotlin
स्वागत है Kotlin संस्करण 1.7.10 में (JRE 1.8.0_292-b10)
मदद के लिए :help टाइप करें, बंद करने के लिए :quit
>>> val greeting = "नमस्ते, Kotlin REPL!"
>>> println(greeting)
नमस्ते, Kotlin REPL!
```

## गहराई में जानकारी
Kotlin का REPL भाषा के साथ प्रयोग करने को प्रोत्साहित करने के लिए शुरू किया गया था। यह Python के इंटरैक्टिव शेल के समान है लेकिन Kotlin के सिंटेक्स और विशेषताओं के लिए अनुकूलित है। विकल्प? IDE में इंटरैक्टिव वातावरण, जैसे कि IntelliJ IDEA, और ऑनलाइन Kotlin प्लेग्राउंड। REPL मौके पर कोड को संकलित करके काम करता है, तत्काल प्रतिक्रिया प्रदान करता है - सीखने और डिबगिंग के लिए महत्वपूर्ण।

## देखें भी
- Kotlin दस्तावेज़ीकरण REPL पर: [https://kotlinlang.org/docs/command-line.html#run-the-repl](https://kotlinlang.org/docs/command-line.html#run-the-repl)
- ब्राउज़र में Kotlin को आज़माएं: [https://play.kotlinlang.org](https://play.kotlinlang.org)
- IntelliJ IDEA के लिए JetBrains Kotlin प्लेग्राउंड प्लगइन।
