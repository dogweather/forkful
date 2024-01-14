---
title:                "Kotlin: Satunnaislukujen luominen"
simple_title:         "Satunnaislukujen luominen"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/kotlin/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Miksi: Satunnaislukujen generoiminen

Satunnaislukujen generointi on tärkeä osa monia ohjelmia, kuten pelejä, simulaatioita ja kryptografiaa. Se mahdollistaa ohjelmien toiminnan ennustamattomuuden ja vaihtelun. 

## Miten: Esimerkkejä Kotlin-koodilla

```Kotlin
// Generoi satunnainen kokonaisluku väliltä 1-10
val randomNumber = (1..10).random()
println("Satunnainen luku väliltä 1-10 on $randomNumber")

// Generoi satunnainen desimaaliluku väliltä 0.0-1.0
val randomDecimal = Math.random()
println("Satunnainen desimaaliluku väliltä 0.0-1.0 on $randomDecimal")

// Generoi satunnainen kirjain merkkijonosta
val letters = "abcdefghijklmnopqrstuvxyz"
val randomLetter = letters.random()
println("Satunnainen kirjain merkkijonosta on $randomLetter")
```

Esimerkeissä käytämme Kotlinin standardikirjaston metodia `random()`, joka palauttaa satunnaisen luvun halutulta alueelta. Huomaa, että `random()` palauttaa eri muotoisia lukuja riippuen, onko sitä käytetty kokonaislukujen, desimaalilukujen vai merkkijonojen kanssa.

## Syventävä tieto: Satunnaislukujen generointi

Satunnaislukujen generointi ei ole täysin satunnaista, vaan se perustuu matemaattisiin algoritmeihin. Esimerkiksi Kotlinin `random()` metodi käyttää taustallaan `java.util.Random` luokkaa. 

On tärkeää huomata, että satunnaislukujen generoiminen ei ole täysin turvallista kryptografisessa yhteydessä, sillä algoritmeja voi ennustaa. Tällöin tulisi käyttää erityisiä kryptografisia kirjastoja, jotka tarjoavat vahvemman satunnaisuuden.

## Katso myös

- [Random-kirjaston dokumentaatio](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.random/index.html)
- [Artikkeli "Satunnaislukujen generointi"](https://www.baeldung.com/kotlin/random-number)
- [Javan Random-luokan dokumentaatio](https://docs.oracle.com/javase/8/docs/api/java/util/Random.html)