---
date: 2024-01-20 17:42:27.769607-07:00
description: "How to: (Kuinka tehd\xE4:) Alun perin merkkijonojen kaavojen k\xE4sittely\
  \ kehitettiin osaksi suurempia, tekstink\xE4sittelyyn tarkoitettuja ohjelmia, kuten\
  \ sed ja\u2026"
lastmod: '2024-04-05T22:51:10.674832-06:00'
model: gpt-4-1106-preview
summary: "(Kuinka tehd\xE4:) Alun perin merkkijonojen kaavojen k\xE4sittely kehitettiin\
  \ osaksi suurempia, tekstink\xE4sittelyyn tarkoitettuja ohjelmia, kuten sed ja awk\
  \ UNIX-j\xE4rjestelmiss\xE4."
title: Merkkien poistaminen hakemalla osumia kaavaan
weight: 5
---

## How to: (Kuinka tehdä:)
```kotlin
fun main() {
    val originalText = "Täällä on esimerkki4 tekstistä, jossa on 3 numeroa."
    val pattern = "\\d".toRegex() // Kaava numeroiden tunnistamiseen
    val cleanedText = originalText.replace(pattern, "")
    println(cleanedText) // Tulostetaan puhdistettu teksti
}
```
Sample output:
```
Täällä on esimerkki tekstistä, jossa on  numeroa.
```

## Deep Dive (Sukellus syvyyksiin)
Alun perin merkkijonojen kaavojen käsittely kehitettiin osaksi suurempia, tekstinkäsittelyyn tarkoitettuja ohjelmia, kuten sed ja awk UNIX-järjestelmissä. Kotlin käyttää Java-perustaisia regular expressions -järjestelmiä. Vaihtoehtoja merkkijonojen käsittelyyn ovat muiden ohjelmointikielten kirjastot tai työkalut, kuten Perl tai Python. Kotlinissa `.replace`-funktio ottaa regulaarilausekkeen ja korvaa kaikki vastaavat osat merkkijonossa annetulla korvaavalla merkkijonolla.

## See Also (Katso myös)
- Kotlinin virallinen dokumentaatio: [Regular Expressions](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/-regex/)
- Java Platform, Standard Edition & Java Development Kit -versio 17 API-määritys: [Pattern](https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/util/regex/Pattern.html)
- [RegExr](https://regexr.com/): Regulaarilausekkeiden opettelun ja testauksen työkalu
- [RegEx101](https://regex101.com/): Toinen hyödyllinen sivusto regulaarilausekkeiden testaamiseen
