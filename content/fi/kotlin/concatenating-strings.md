---
title:                "Merkkijonojen yhdistäminen"
html_title:           "Gleam: Merkkijonojen yhdistäminen"
simple_title:         "Merkkijonojen yhdistäminen"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/kotlin/concatenating-strings.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?

Merkkijonojen yhdistäminen eli konkatenaatio on se, kun kaksi tai useampi merkkijono yhdistetään lomittain. Ohjelmoijat tekevät tätä, koska se on käytännöllinen tapa muodostaa uusia merkkijonoja tai manipuloida olemassa olevia merkkijonoja.

## Näin se tehdään:

Kotlinissa voit yhdistää merkkijonoja "+"-operaattorin avulla. Tässä esimerkki:

```Kotlin
fun main() {
    val s1 = "Hei, "
    val s2 = "maailma!"
    val yhdistetty = s1 + s2
    println(yhdistetty)  //Tulostaa: Hei, maailma!
}
```

## Syvempi sukellus:

Ennen Javan SE 5 -versiota, merkkijonojen konkatenoinnin tehokkain tapa oli käyttää StringBuffer-luokkaa. Kotlin perineekin Javan String- ja StringBuilder-luokkien käyttökäytännöt. Nykyään voimme käyttää "+"-operaattoria tai "plus"-metodia merkkijonoihin helposti ja tehokkaasti Kotlinissa.

Uusi ja tehokas tapa tehdä merkkijonojen konkatenointi on "string templates". Ne mahdollistavat muuttujien arvojen sisällyttämisen merkkijonojen sisällä.

```Kotlin
fun main() {
    val nimi = "Matti"
    println("Hei, $nimi")  //Tulostaa: Hei, Matti
}
```

On tärkeää tietää, että suurten määrien merkkijonojen yhdistäminen yhden "+"-operaattorin avulla voi hidastaa ohjelman suorituskykyä, koska jokainen "+"-operaatio luo uuden String-olion. Siksi tehokkaampi vaihtoehto on käyttää StringBuilder- tai StringBuffer-luokkaa isojen määrien merkkijonojen yhdistämisessä.

## Katso myös:

1. [Kotlinin virallinen dokumentaatio merkkijonon konkatenoinnista](https://kotlinlang.org/docs/strings.html)
2. [StringBuilder-luokka Kotlinissa](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/-string-builder/)
3. [Merkkijonojen käyttö Kotlinissa](https://medium.com/@fatihcoskun/kotlin-strings-9b7f6cd3f5d0)