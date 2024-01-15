---
title:                "Merkkijonon pituuden laskeminen"
html_title:           "Kotlin: Merkkijonon pituuden laskeminen"
simple_title:         "Merkkijonon pituuden laskeminen"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/kotlin/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Miksi

Miksi olisit kiinnostunut kaltaisen lehdykän pituuden löytämisestä? No, on monia syitä! Kenties haluat muokata tai tarkistaa tekstin pituuden ennen sen lisäämistä johonkin toiseen ohjelmaan. Tai ehkä se on vain yleinen ohjelmointitaito, jota haluat harjoitella. Jatka lukemista löytääksesi yksinkertaisen tavan löytää merkkijonon pituus Kotlin-kielellä.

## Kuinka

Kotlin tarjoaa helpon ja suoraviivaisen tavan löytää merkkijonon pituus. Käytä vain ".length" -ominaisuutta ja se palauttaa merkkijonon pituuden. Alla on esimerkki:

```Kotlin
fun main() {
    val name = "Kotlin"
    println("Merkkijonon \"$name\" pituus on ${name.length}")
}
```
Tämä koodi tulostaa "Merkkijonon "Kotlin" pituus on 6". Kuten näet, käytämme ".length" -ominaisuutta vain merkkijonon nimen perää. Voit myös tallentaa merkkijonon pituuden muuttujaan ja käyttää sitä myöhemmin tarvittaessa.

## Syvällisemmin

Tarkenna vielä enemmän, ".length" on ominaisuus, jota käytetään kaikilla Kotlinin merkkijonotyypeillä. Se ei ole toiminto, ja siksi se ei sulkeudu "()" -sulkeisiin. Tämä tekee siitä hieman erilaisen muihin kielten sisäänrakennettuihin merkkijonotoimintoihin verrattuna. Lisäksi muista, että ".length" kertoo sinulle merkkijonon pituuden, ei sisällön määrää. Esimerkiksi "Hello" on viisi merkkiä pitkä, mutta seuraava koodi tulostaa "Merkkijonon "Hello" pituus on 5", ei "Merkkijonon "Hello" pituus on 1".

```Kotlin
fun main() {
    val greeting = "Hello"
    println("Merkkijonon \"$greeting\" pituus on ${greeting.length}")
}
```

## Katso myös

- [Merkkijonon pituuden löytäminen Java-kielellä](https://www.javatpoint.com/how-to-find-string-length-in-java)
- [Kotlinin viralliset ohjeet merkkijonoista](https://kotlinlang.org/docs/reference/basic-types.html#strings)
- [Merkkijonon manipulointi Kotlinilla](https://www.geeksforgeeks.org/kotlin-string-manipulation/)