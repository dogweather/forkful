---
date: 2024-01-20 17:42:30.709007-07:00
description: "Pattern matching se hum kisi string mein se khas characters ya shabdon\
  \ ko dhundhkar unhe hata sakte hain. Programmers iska use karte hain data ko saf\u2026"
lastmod: '2024-03-13T22:44:52.231208-06:00'
model: gpt-4-1106-preview
summary: Pattern matching se hum kisi string mein se khas characters ya shabdon ko
  dhundhkar unhe hata sakte hain.
title: "\u092A\u0948\u091F\u0930\u094D\u0928 \u0938\u0947 \u092E\u0947\u0932 \u0916\
  \u093E\u0924\u0947 \u0905\u0915\u094D\u0937\u0930\u094B\u0902 \u0915\u094B \u0939\
  \u091F\u093E\u0928\u093E"
weight: 5
---

## How to (कैसे करें):
```kotlin
fun main() {
    val originalText = "Programming in Kot1in is fun2!"
    val pattern = "\\d".toRegex() // Digits ko dhundhne ke liye pattern
    val sanitizedText = originalText.replace(pattern, "")
    println(sanitizedText) // Output: Programming in Kotlin is fun!
}
```
Is code mein, `\\d` ka matlab hota hai "koi bhi digit". `toRegex()` ko use karke hum is pattern ko regex mein convert karte hain. `replace()` function se hum original text mein se sabhi digits ko hata dete hain.

## Deep Dive (गहराई से जानिये):
Pattern matching ka istemaal bahut pehle se hota aaya hai. Unix-based systems mein `grep` jaisi command-line tools iska ek udaharan hain. Kotlin mein regex ka use karke character matching aur replacing bohot aasan ban gaya hai. Alternatives mein `filter` aur lambdas bhi hote hain, par regex zyada powerful hota hai khas kar jab complex patterns ki baat aati hai. Regex implementation details ke liye, Kotlin Java ki `Pattern` aur `Matcher` classes ka sahara leta hai.

## See Also (और देखें):
- Kotlin documentation on regex: [Kotlin Regex](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/-regex/)
- Java Pattern class: [Java Pattern](https://docs.oracle.com/javase/7/docs/api/java/util/regex/Pattern.html)
- Tutorial on regular expressions: [Regex Tutorial](https://www.regular-expressions.info/tutorial.html)
