---
title:                "Onko hakemisto olemassa? Tarkistaminen"
date:                  2024-01-20T14:57:30.415272-07:00
html_title:           "Gleam: Onko hakemisto olemassa? Tarkistaminen"
simple_title:         "Onko hakemisto olemassa? Tarkistaminen"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/kotlin/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## What & Why? (Mitä & Miksi?)
Tarkistetaan, onko kansio olemassa tiedostojärjestelmässä. Koodarit tekevät tämän välttääkseen virheitä, kuten yrittää kirjoittaa olemattomaan kansioon.

## How to: (Kuinka tehdään:)
```kotlin
import java.nio.file.Files
import java.nio.file.Paths

fun main() {
    val path = Paths.get("/polku/kansioon")
    val exists = Files.exists(path)

    println("Onko kansio olemassa? $exists")
}

// Esimerkkituloste:
// Onko kansio olemassa? true
```

## Deep Dive (Sukellus syvyyksiin)
Kansioita on tarkistettu olemassaolonsa puolesta kauan, koska se on olennainen osa tiedostonhallintaa. Ennen Java NIO:t (New I/O), File-luokkaa käytettiin, mutta se ei ollut yhtä tehokas eikä yhtä monipuolinen. `Files.exists()` tuli käyttöön Java 7:ssä osana NIO.2:ta, tarjoten tehokkaamman tavan tehdä tämä tarkistus. Vaihtoehtoisesti `Files.isDirectory(path)` voidaan käyttää suoraan, jos odotetaan, että polku on aina kansio. Implementation yksityiskohdissa on hyvä pitää mielessä, että `Files.exists()` voi hitaasti reagoida verkkolevyillä ja voi palauttaa `false`, jos käyttöoikeudet puuttuvat, vaikka kansio olisikin olemassa.

## See Also (Katso Myös)
- Kotlinin virallinen dokumentaatio: [https://kotlinlang.org/docs/home.html](https://kotlinlang.org/docs/home.html)
- StackOverflow keskustelut ja esimerkit tiedon käsittelystä Kotlinissa: [https://stackoverflow.com/questions/tagged/kotlin+file-io](https://stackoverflow.com/questions/tagged/kotlin+file-io)
