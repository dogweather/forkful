---
title:                "स्टैंडर्ड त्रुटि पर लिखना"
html_title:           "Kotlin: स्टैंडर्ड त्रुटि पर लिखना"
simple_title:         "स्टैंडर्ड त्रुटि पर लिखना"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/kotlin/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## क्यों

कोटलिन कोडिंग में, बहुत बार हमें प्रोग्राम को एक निर्दिष्ट त्रुटि के साथ सम्बंधित जानकारी को लोगों को देने की आवश्यकता होती है। इसके लिए हम वैसे ही कोड लिखते हैं जैसे हम स्टैंडर्ड आउटपुट को प्रिंट करते हैं, लेकिन इस स्थिति में हम स्टैंडर्ड एरर को प्रिंट करते हैं। जिससे कॉन्सोल पर हम स्पष्ट रूप से दिखा सकते हैं कि प्रोग्राम किस त्रुटि का सामना कर रहा है और उसका सामान्य उपचार क्या हो सकता है। इसलिए कोडिंग में यह जरूरी है कि हम स्टैंडर्ड एरर प्रोग्राम करें।

## कैसे करें

कोटलिन में स्टैंडर्ड एरर प्रिंट करने के लिए, हम `System.err` ऑब्जेक्ट का उपयोग कर सकते हैं। इसके लिए हम `println()` फंक्शन को `System.err` के साथ इस्तेमाल कर सकते हैं। नीचे दिए गए कोड ब्लॉक में एक सरल उदाहरण है:

```Kotlin
fun main() {
    println("Starting program...")
    val num : Int = "Hello".toInt()  //This line will throw an error 
    println("Program completed successfully.")
}
```

जैसा कि आप देख सकते हैं, `toInt()` फंक्शन `String` को `Int` में कनवर्ट करने की कोशिश करता है, जो कि गलत होने का संदेश (`NumberFormatException`) देता है। इसके बाद, हम `println()` का उपयोग करके `System.err` पर यह संदेश प्रिंट करते हैं:

```Kotlin
fun main() {
    println("Starting program...")
    try {
        val num : Int = "Hello".toInt()
    } catch(e : NumberFormatException) {
        println("Error while converting string to number: " + e.message)
    }
    println("Program completed successfully.")
}
```

आप ऊपर दिए गए कोड ब्लॉक को कॉपी पेस्ट करे और रन करें तो आपको यह मैसेज(onsole पर दिखेगा:

```
Starting program...
Program completed successfully.
```