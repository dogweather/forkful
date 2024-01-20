---
title:                "पैटर्न से मिलते जुलते वर्णों को हटाना"
html_title:           "Elixir: पैटर्न से मिलते जुलते वर्णों को हटाना"
simple_title:         "पैटर्न से मिलते जुलते वर्णों को हटाना"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/kotlin/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## क्या एवं क्यों?

पैटर्न मैच करने वाले वर्णों को डिलीट करना एक आम कार्य होता है जहां हम एक निश्चित पैटर्न मैच करने वाले वर्णों को एक String से हटा देते हैं। प्रोग्रामर्स इसे डाटा क्लीनिंग के उद्देश्य से करते हैं, जैसे की अनवांछित वर्णों को हटाना हो सकता है।

## कैसे करें:

आइए देखें कि Kotlin में यह कैसे किया जा सकता है:

```Kotlin 
fun main() { 
    val str = "Hello, World!"
    val pattern = "[, ]"   // हमें कौन से वर्ण हटाने हैं उन्हें यहाँ निर्दिष्ट करें
    val removed = str.replace(Regex(pattern), "")
    println(removed)
}
```

ऊपरी कोड का आउटपुट निम्नलिखित होगा:

```
HelloWorld!
```

"Hello, World!" String से हमने comma और space वर्णों को हटा दिया है।

## गहराई में:

इतिहासकारी में यह कार्य पाठ एडिटिंग सिस्टम्स, जैसे अक्स और सेड, का भाग था। Kotlin में, `replace()` फंक्शन Regex और replacement String का उपयोग करती है।

विकल्प स्वरूप, आपके पास अन्य लाइब्रेरीज या फंक्शन्स का उपयोग करने का विकल्प हो सकता है, जैसे जावा का `replaceAll()` या कस्टम कोड। याद रखें, `replace()` केवल पहले से निर्दिष्ट पैटर्न को हटाता है, इसलिए इसका उपयोग करते वक्त सतर्क रहें।

## अन्य स्रोतों के लिए:

2. [Java String replaceAll()](https://docs.oracle.com/en/java/javase/14/docs/api/java.base/java/lang/String.html#replaceAll(java.lang.String,java.lang.String))

इन स्रोतों में आपको String के साथ काम करते समय नियमबद्धताओं और विस्तार की जानकारी मिलेगी।