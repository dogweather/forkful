---
title:                "Virheenjäljitystulosteiden tulostaminen"
date:                  2024-01-20T17:53:02.470591-07:00
model:                 gpt-4-1106-preview
simple_title:         "Virheenjäljitystulosteiden tulostaminen"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/kotlin/printing-debug-output.md"
---

{{< edit_this_page >}}

## What & Why?
Mikä ja miksi? Debug-tulosteiden tulostaminen tarkoittaa koodin toiminnan tarkkailua reaaliajassa. Kehittäjät käyttävät sitä sovellusten virheiden etsimiseen ja korjaamiseen.

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
