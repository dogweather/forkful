---
title:                "Sattumanvaraisten lukujen generointi"
html_title:           "Kotlin: Sattumanvaraisten lukujen generointi"
simple_title:         "Sattumanvaraisten lukujen generointi"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/kotlin/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Miksi

Satunnaisten numeroiden generoiminen on tärkeää osa useita ohjelmia ja sovelluksia. Se voi auttaa simuloinnissa, testaamisessa tai jopa parantaa käyttäjäkokemusta jännityksen ja vaihtelun avulla.

## Miten

Jos haluat generoida satunnaisia numeroita käyttäen Kotlinia, sinun tulee ensin tuoda `kotlin.random` kirjasto käyttöön. Sitten voit käyttää `nextInt()` funktiota, joka palauttaa satunnaisen numeron annetulla välillä, kuten alla olevassa esimerkissä:

```Kotlin
import kotlin.random.Random

val random = Random(42) // asetetaan siemenluku
val number = random.nextInt(1, 10) // generoidaan luku väliltä 1-9
println(number) // tulostaa esimerkiksi: 5
```

Voit myös generoida merkkijonoja käyttäen `nextBytes()` funktiota, joka palauttaa satunnaisen tavujonon annetun pituuden mukaan. Esimerkiksi:

```Kotlin
val bytes = random.nextBytes(4) // generoi 4 tavun pituisen merkkijonon
println(bytes.contentToString()) // tulostaa esimerkiksi: [113, 11, 97, 14]
```

## Syvemmälle

Satunnaisia numeroita generoidessa on tärkeää ymmärtää käytettyä siemenlukua. Siemenluvun avulla generointi on toistettavissa, joten jos käytät samaa siemenlukua, saat saman sarjan satunnaisia numeroita. Toisaalta, jos siemenluku on eri, saat myös erilaisen sarjan satunnaisia numeroita.

Voit myös asettaa oman siemenluvun tai käyttää oletusarvoista järjestystä käyttämällä `Random.Default` -funktiota.

On myös tärkeää huomata, että satunnaisuutta ei voi taata lopullisessa tuotantoversiossa, vaan se on matemaattinen järjestelmä, joten jotkut numerot saattavat esiintyä useammin kuin toiset. Tästä syystä ei ole suositeltavaa käyttää satunnaisia numeroita esimerkiksi salasanoiden generoimisessa.

## Katso myös

- [Kotlinin virallinen dokumentaatio](https://kotlinlang.org/docs/reference/basic-types.html#random-numbers)
- [Java Random- ja SecureRandom -luokat](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/util/Random.html)
- [Randomness in Programming: Why Should You Care?](https://www.freecodecamp.org/news/randomness-in-programming-why-should-you-care-aa8fd2183e7e/)