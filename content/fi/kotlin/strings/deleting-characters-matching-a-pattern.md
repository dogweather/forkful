---
date: 2024-01-20 17:42:27.769607-07:00
description: "Kun poistetaan merkkej\xE4, jotka vastaavat tietty\xE4 kaavaa, siivotaan\
  \ merkkijonoa tarpeettomasta sis\xE4ll\xF6st\xE4. T\xE4m\xE4 on hy\xF6dyllist\xE4\
  , jotta voidaan esimerkiksi\u2026"
lastmod: '2024-02-25T18:49:53.432764-07:00'
model: gpt-4-1106-preview
summary: "Kun poistetaan merkkej\xE4, jotka vastaavat tietty\xE4 kaavaa, siivotaan\
  \ merkkijonoa tarpeettomasta sis\xE4ll\xF6st\xE4. T\xE4m\xE4 on hy\xF6dyllist\xE4\
  , jotta voidaan esimerkiksi\u2026"
title: Merkkien poistaminen hakemalla osumia kaavaan
---

{{< edit_this_page >}}

## What & Why? (Mikä & Miksi?)
Kun poistetaan merkkejä, jotka vastaavat tiettyä kaavaa, siivotaan merkkijonoa tarpeettomasta sisällöstä. Tämä on hyödyllistä, jotta voidaan esimerkiksi puhdistaa käyttäjän syötteet tai muotoilla dataa jatkokäsittelyä varten.

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
