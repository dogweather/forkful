---
title:                "Alimerkkijonojen poiminta"
html_title:           "Gleam: Alimerkkijonojen poiminta"
simple_title:         "Alimerkkijonojen poiminta"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/kotlin/extracting-substrings.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?

Alimerkkijonon poiminta on toiminto, jolla voidaan hakea osa merkkijonosta. Ohjelmoijat käyttävät tätä ominaisuutta erityisesti datan manipulointiin ja jäsentämiseen.

## Kuinka näin:

Alla esimerkkejä Kotlin-koodista ja sen tulosteista. Käytämme `substring` -funktiota.

```Kotlin
fun main() {
    val text = "Tervetuloa Kotlin-ohjelmointiin!"
    println(text.substring(0, 10)) // Tulostaa: "Tervetuloa"
}
```
Tämä pala koodia tulostaa merkkijonon "Tervetuloa Kotlin-ohjelmointiin!" ensimmäiset 10 merkkiä, eli "Tervetuloa".

```Kotlin
fun main() {
    val text = "Tervetuloa Kotlin-ohjelmointiin!"
    println(text.substringAfter(" ")) // Tulostaa: "Kotlin-ohjelmointiin!"
}
```
Tämä koodinpätkä etsii ensimmäisen välilyönnin jälkeisen merkkijonon "Tervetuloa Kotlin-ohjelmointiin!" ja tulostaa sen, eli "Kotlin-ohjelmointiin!".

## Syvempi Sukellus:

#### Historiallinen Konteksti:
Alimerkkijonon poiminta on perusominaisuus useimmissa ohjelmointikielissä. Myös Kotlinin varhaisissa versioissa tämä ominaisuus on ollut jo käytössä.

#### Vaihtoehdot:
Voit käyttää myös `substringBefore` ja `substringAfterLast` funktioita, jotka ovat samankaltaisia mutta eroavat käyttötavoissaan.

#### Toteutusyksityiskohdat:
Kotlin käyttää Javan `String` -luokkaa ja sen `substring` -metodia alimerkkijonotoiminnoissa. Javan virtuaalikoneen ansiosta tämä on erittäin tehokasta.

## Katso Myös:

- Kotlinin virallinen dokumentaatio alimerkkijonoista: [Kotlin substring Doc](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/substring.html)
- Stack Overflow's discussion on substring functions: [Stack Overflow Discussions](https://stackoverflow.com/questions/36574183/how-to-substring-in-kotlin)
- Online Kotlin compiler for testing code: [Kotlin Playground](https://play.kotlinlang.org/)