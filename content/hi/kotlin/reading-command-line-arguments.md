---
title:                "कमांड लाइन तर्कों को पढ़ना"
html_title:           "Kotlin: कमांड लाइन तर्कों को पढ़ना"
simple_title:         "कमांड लाइन तर्कों को पढ़ना"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/kotlin/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

Command Line Arguments क्या हैं और क्यों प्रोग्रामर्स इसे इस्तेमाल करते हैं? कमांड लाइन arguments वे parameters होते हैं जिसे मुख्य फंक्शन को पास किया जाता है। ये main() फंक्शन की flexibility बढ़ाते हैं और उपयोगकर्ताओं को नियंत्रण देते हैं।

## कैसे करें:

आपको बस `Array<String>` पैरामीटर का उपयोग करने की जरूरत होती है, जैसे निम्नलिखित कोड में दिखाया गया है।

```Kotlin
fun main(args: Array<String>) {
    if (args.isNotEmpty()) {
        for (arg in args) {
            println("Argument: $arg")
        }
    } else {
        println("No arguments found.")
    }
}
```

आउटपुट के लिए अगर हम इस प्रोग्राम को `Kotlin ArgsDemoKt A B C` के साथ चलाते हैं:

```bash
Argument: A
Argument: B
Argument: C
```

## गहराई में:

Command Line Arguments का इस्तेमाल C और C++ में शुरू हो गया था लेकिन इसकी क्षमता Kotlin में बढ़ गई है। वैकल्पिक इंपलीमेंटेशन के तौर पर, आप `JCommander` जैसे libraries का इस्तेमाल कर सकते हैं जो आपको और ज़्यादा पावर और फ्लेक्सिबिलिटी देती हैं। एक बात ध्यान देने योग्य है कि आपके arguments का क्रम महत्वपूर्ण हो सकता है। यदि किसी एप्लिकेशन के arguments का क्रम गलत हो जाए तो प्रोग्राम अप्रत्याशित रूप से व्यवहार कर सकता है।

## अधिक जानकारी के लिए:

1. [Command Line Arguments in Kotlin](https://kotlinlang.org/docs/command-line.html)
2. [Arguments Processing using JCommander](http://jcommander.org/)
3. [Kotlin's Main Function](https://kotlinlang.org/docs/functions.html)