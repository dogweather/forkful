---
title:                "स्ट्रिंग को कैपिटलाइज करना"
html_title:           "Kotlin: स्ट्रिंग को कैपिटलाइज करना"
simple_title:         "स्ट्रिंग को कैपिटलाइज करना"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/kotlin/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## क्या है और क्यों?: 

1. एक string को capitalize करने का मतलब होता है हर शब्द की पहली अक्षर को बड़े (Capital) अक्षर में परिवर्तित करना। 
2. यह super user और basic user के बीच अंतर स्थापित करने में सहायक होता है, और यह user interface को व्यावसायिक और आकर्षक बनाता है। 

## कैसे करें:

```Kotlin
fun main() {
  val string = "hello, कैसे है आप?"
  val result = string.split(" ").joinToString(" ") { it.capitalize() }
  println(result)  
}
```

ऊपर दिए गए कोड से आपको निम्नलिखित आउटपुट मिलेगा

```Kotlin
Hello, कैसे है आप?
```

## गहराई का अध्ययन :

1. string को capitalize करनेका अभ्यास 1970 के दशक के उत्कृष्ट computer user interfaces (UIs) के साथ आया था। 
2. Kotlin में "capitalize" function के इलावा, आप "toUpperCase" और "toLowerCase" function का भी use कर सकते हैं। 
3. string को capitalize करने के लिए, हम वाक्य में हर शब्द को अलग करने के लिए split() function का उपयोग करते हैं, और फिर हर शब्द को join करते हैं ।

## अधिक जानने के लिए:

1. String capitalize documentation: https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/capitalize.html 
2. String toUpperCase function: https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/to-upper-case.html 
3. String toLowerCase function: https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/to-lower-case.html