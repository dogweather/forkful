---
title:                "पैटर्न से मेल खाते हुए अक्षरों को हटाना"
html_title:           "Kotlin: पैटर्न से मेल खाते हुए अक्षरों को हटाना"
simple_title:         "पैटर्न से मेल खाते हुए अक्षरों को हटाना"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/kotlin/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
character matching pattern को हटाना क्या है और programming में इसे क्यों किया जाता है।

कई बार programming में हमे एक विशिष्ट पैटर्न के अनुसार characters को हटाना होता है। इससे हमे दूसरे characters को ढूंढने में सहायता मिलती है जो कि एक लंबे समय तक खोज के लिए जरूरी हो सकते हैं। 

## कैसे करें:
कोटलिन में character matching pattern को हटाने का प्रोग्राम निम्न तरीके से संचालित किया जा सकता है।

```
fun main() {
   val str = "Hello World"
   val pattern = "[a-z]".toRegex()
   val result = str.replace(pattern, "")
   println(result) 
}

// Output: H W
```

इसके अलावा, जिस character को हम हटाना चाहते हैं, उसके साथ और operations भी किये जा सकते हैं। जैसे की अगर हम अभिव्यक्ति में एक एक्स्ट्रा स्पेस भी हटाना चाहते हैं तो निम्न तरीके से कर सकते हैं। 

```
val result = str.replace(pattern, "").trim()
// Output: H W
```

## गहराई में:
character matching pattern को हटाने का काम कई दशकों से हमने pay किया है। कोटलिन में भी इसे अनगिनत तरीकों से संचालित किया जा सकता है जैसे builtin function ```removeIf```, ```dropWhile``` और ```filterNot```। कई अन्य programming languages में इसे regular expressions के साथ भी किया जाता है। 

इसके अलावा, हम character matching pattern को हटाने में glob patterns भी प्रयोग करते हैं। इनकी मदद से हम अनुकूलन को भी कर सकते हैं जैसे कि case sensitivity में अंतर। 

## इससे जुड़े:
- [Kotlin RemoveSpecificCharacter][1]
- [Kotlin String documentation][2]
- [Kotlin Collection operations][3]

[1]: https://kotlinbyshivank.blogspot.com/2017/04/kotlin-remove-specific-character.html
[2]: https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-string/
[3]: https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.collections/index.html