---
title:                "स्ट्रिंग्स को जोड़ना"
html_title:           "Bash: स्ट्रिंग्स को जोड़ना"
simple_title:         "स्ट्रिंग्स को जोड़ना"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/kotlin/concatenating-strings.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

'String Concatenation' या 'स्ट्रिंग जोड़ना' कॉडिंग में ऐसी प्रक्रिया है जिसमें कई strings को हम एक साथ जोड़ते हैं ताकि एक नई स्ट्रिंग बन सके।  यह तब किया जाता है जब प्रोग्रामर को कई अलग-अलग सूचना संघटित रूप से प्रदर्शित करने की जरूरत होती है।

## कैसे करें:

Kotlin में string concatenation करना बहुत सरल होता है। निम्नलिखित कोड का परीक्षण करें:

```Kotlin
fun main() {
    val str1 = "नमस्ते, "
    val str2 = "दुनिया!"
    val greeting = str1 + str2
    println(greeting)
}
```

इसका आउटपुट होगा:

```
नमस्ते, दुनिया!
```

## गहराई :

String concatenation की स्थापना प्रोग्रामिंग के साथ साह-साथ शुरू हुई। यह एक साधारण और स्पष्टता प्रदान करने वाली तकनीक है जिसका उपयोग आमतौर पर उपयोगकर्ता को समझाने या निर्देशित करने के लिए करते हैं।

Kotlin में string interpolation के माध्यम से भी string concatenation किया जा सकता है। यह एक ऐसी विधि है जिसमें expressions को String literals के अंदर एंबेड किया जा सकता है:

```Kotlin
fun main() {
    val world = "दुनिया"
    println("नमस्ते, $world")
}
```

यह कोड भी वही आउटपुट देगा:

```
नमस्ते, दुनिया!
```

## और देखें :

अधिक जानकारी के लिए, कृपया निम्नलिखित रिसोर्सेस का अन्वेषण करें:

1. Kotlin के [official documentation](https://kotlinlang.org/docs/reference/basic-types.html#string-literals) की जांच करें।