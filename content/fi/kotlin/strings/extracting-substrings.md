---
date: 2024-01-20 17:45:56.829704-07:00
description: "How to: Voit my\xF6s k\xE4ytt\xE4\xE4 range-operaattoreita."
lastmod: '2024-04-05T21:53:58.087061-06:00'
model: gpt-4-1106-preview
summary: "Voit my\xF6s k\xE4ytt\xE4\xE4 range-operaattoreita."
title: Merkkijonojen osien poimiminen
weight: 6
---

## How to:
``` Kotlin
val fullString = "Hello, mahtava Kotlin!"
val extractedSubstring = fullString.substring(7, 14)
println(extractedSubstring) // tulostaa: mahtava
```
Voit myös käyttää range-operaattoreita:
``` Kotlin
val rangeSubstring = fullString.slice(7..13)
println(rangeSubstring) // sama tulos: mahtava
```
Tai alkupään ilmaisemiseen:
``` Kotlin
val startSubstring = fullString.substring(0..4)
println(startSubstring) // Hello
```

## Deep Dive
Substring-toimintoja on käytetty aivan ohjelmoinnin alkuaikoina. Kotlinissa, kuten Javassa, metodi `substring` kuuluu String-luokkaan. Vaihtoehtoisesti, `slice` antaa jouston käyttää rangetoimintoa. On tärkeää huomioida, että indeksit Kotlinissa alkavat nollasta ja päättyvät n-1.

## See Also
Kotlinin virallinen dokumentaatio substringeista: [Kotlin Substring Documentation](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/substring.html)
Java-stringien käsittelyä seikkaperäisemmin: [Oracle Java Strings](https://docs.oracle.com/javase/7/docs/api/java/lang/String.html)
