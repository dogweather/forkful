---
title:                "डीबग आउटपुट प्रिंट करना"
html_title:           "Gleam: डीबग आउटपुट प्रिंट करना"
simple_title:         "डीबग आउटपुट प्रिंट करना"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/kotlin/printing-debug-output.md"
---

{{< edit_this_page >}}

# डीबग आउटपुट को कोटलिन में प्रिंट करें (Print Debug Output in Kotlin)

## क्या और क्यों?
डीबग आउटपुट प्रिंट करना का मतलब है की आप प्रोग्राम के चलाने के दौरान कार्रवाई को ट्रैक करने केलिए डाटा दिखाते हैं। हम इसे करने के लिए अपनी कोड की समस्याओं को जल्दी से खोजने और समझने के लिए प्रयोग करते हैं।

## कैसे करें:
```Kotlin
fun main() {
    val str = "Debug output"

    println(str) 
}
```
सैंपल आउटपुट:
```
Debug output
```
## गहराई में:
प्रिंट डीबग का इतिहास उन दिनों से शुरू हुआ जब कंप्यूटर कोडिंग और डीबगिंग की दुनिया में नई-नई खोज हो रही थी। आज भी, यह एक बहुत ही उपयोगी उपकरण हैं जब बात कोड की समस्याओ की पहचाने और समझने की आती हैं।

एल्टरनेटिव्स में डीबगर लाइब्रेरी जैसे `Log4j` और `SLF4J` का उपयोग कर सकते हैं जो अधिक फीचर्स देते हैं।

`println` स्टैंडर्ड आउटपुट स्ट्रीम में प्रिंट करता है, जो की डीफॉल्ट रूप से कांसोल में होता है। यह `System.out.print()` का उपयोग करके इसे करता हैं।

## और देखें:
कोटलिन के अधिकारिक दस्तावेज़ : https://kotlinlang.org/docs/reference/basic-syntax.html#printing-to-the-console

Log4j : https://logging.apache.org/log4j/2.x/

SLF4J: http://www.slf4j.org/