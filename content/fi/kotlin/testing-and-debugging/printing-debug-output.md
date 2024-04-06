---
date: 2024-01-20 17:53:02.470591-07:00
description: "How to: Syv\xE4luotaus. Aikaisemmin kehitt\xE4j\xE4t saattoivat k\xE4\
  ytt\xE4\xE4 log-tiedostoja tai jopa konsolin tulosteita virheenkorjaukseen. Kotlinissa\
  \ println on\u2026"
lastmod: '2024-04-05T22:51:10.691497-06:00'
model: gpt-4-1106-preview
summary: "Syv\xE4luotaus. Aikaisemmin kehitt\xE4j\xE4t saattoivat k\xE4ytt\xE4\xE4\
  \ log-tiedostoja tai jopa konsolin tulosteita virheenkorjaukseen. Kotlinissa println\
  \ on yksinkertaisin tapa tulostaa, mutta se ei ole ainoa tapa. Logcat Androidissa\
  \ ja Logger-luokkia JVM-ohjelmissa tarjoavat kontrolloidumman tavan k\xE4sitell\xE4\
  \ lokiviestej\xE4. Kotlinissa `println` on sidottu Java's `System.out`-, `System.err`-virtaan\
  \ ja voi heikent\xE4\xE4 suorituskyky\xE4 tuotannossa. Siksi tuotantokoodissa tulisi\
  \ k\xE4ytt\xE4\xE4 erikoistuneita kirjastoja, kuten SLF4J tai Logback, joissa voit\
  \ m\xE4\xE4ritt\xE4\xE4 logitason."
title: "Virheenj\xE4ljitystulosteiden tulostaminen"
weight: 33
---

## How to:
Koodiesimerkit ja tulosteet.

```Kotlin
fun main() {
    val debugMessage = "Hello, Debug!"
    println(debugMessage)
    // Tulostaa: Hello, Debug!
    
    val errorLevel = "LOW"
    val errorInfo = "Something went wrong..."
    println("[$errorLevel] $errorInfo")
    // Tulostaa: [LOW] Something went wrong...
}
```

## Deep Dive
Syväluotaus. Aikaisemmin kehittäjät saattoivat käyttää log-tiedostoja tai jopa konsolin tulosteita virheenkorjaukseen. Kotlinissa println on yksinkertaisin tapa tulostaa, mutta se ei ole ainoa tapa. Logcat Androidissa ja Logger-luokkia JVM-ohjelmissa tarjoavat kontrolloidumman tavan käsitellä lokiviestejä. Kotlinissa `println` on sidottu Java's `System.out`-, `System.err`-virtaan ja voi heikentää suorituskykyä tuotannossa. Siksi tuotantokoodissa tulisi käyttää erikoistuneita kirjastoja, kuten SLF4J tai Logback, joissa voit määrittää logitason.

## See Also
Lisätietoja. Kattavampaa tietoa löydät:

- Kotlinin virallinen dokumentaatio: [https://kotlinlang.org/docs/home.html](https://kotlinlang.org/docs/home.html)
- SLF4J-projekti: [http://www.slf4j.org/](http://www.slf4j.org/)
