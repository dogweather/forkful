---
title:                "Merkkien poistaminen hakemalla osumia kaavaan"
date:                  2024-01-20T17:42:27.769607-07:00
model:                 gpt-4-1106-preview
simple_title:         "Merkkien poistaminen hakemalla osumia kaavaan"

category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/kotlin/deleting-characters-matching-a-pattern.md"
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
