---
title:                "Tarkistetaan, onko hakemisto olemassa"
html_title:           "Kotlin: Tarkistetaan, onko hakemisto olemassa"
simple_title:         "Tarkistetaan, onko hakemisto olemassa"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/kotlin/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Miksi tarkistaa, jos hakemisto on olemassa?

Joskus ohjelmoidessamme, meidän on tarkistettava, onko tietty hakemisto olemassa joko luodaksemme sen tai suorittaaksemme tiettyjä toimintoja vain, jos se on jo olemassa.

## Kuinka tehdä se

```Kotlin
val directory = File("polku/hakemistoon")

if (directory.exists()) {
    println("Hakemisto on jo olemassa.")
} else {
    println("Hakemisto ei ole vielä olemassa.")
}
```

Koodinpätkä tarkistaa ensin, onko "polku/hakemistoon" nimistä hakemistoa olemassa ja tulostaa sitten vastaavan viestin riippuen tuloksesta.

## Syvemmälle

Voit tarkistaa myös, onko hakemistossa tiettyä tiedostoa käyttämällä `.isDirectory()` -funktiota tarkistamaan, että se on todellakin hakemisto ja ei esimerkiksi tiedosto.

```Kotlin
val directory = File("polku/hakemistoon")
val file = File("polku/hakemistoon/tiedosto.txt")

if (directory.isDirectory()) {
    println("Hakemisto on jo olemassa.")
} else {
    println("Hakemisto ei ole vielä olemassa.")
}

if (file.exists()) {
    println("Tiedosto on olemassa hakemistossa.")
} else {
    println("Tiedostoa ei ole olemassa hakemistossa.")
}
```

## Katso myös

- [Kotlin dokumentaatio](https://kotlinlang.org/docs/getting-started.html)
- [Kotlin virallinen opetusohjelma](https://kotlinlang.org/docs/tutorials/)
- [Kotlin Standard Library](https://kotlinlang.org/api/latest/jvm/stdlib/index.html)