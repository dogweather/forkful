---
title:                "Kotlin: Satunnaisten lukujen luominen"
programming_language: "Kotlin"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/kotlin/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Miksi
On monia syitä miksi voit haluta generoida satunnaisia numeroita ohjelmoinnissa. Esimerkiksi peleissä, tarvitaan usein satunnaisia lukuja eri ominaisuuksien, kuten vihollisten spawnauksen tai palkintojen sijainnin, määrittämiseen.

## Miten
Kotlinissa, satunnaisia lukuja voi generoida käyttämällä `Random` luokkaa ja sen eri metodeja. Alla on muutamia esimerkkejä koodista ja niiden tulostuksista.

```Kotlin
// Generoidaan satunnainen kokonaisluku väliltä 1-10
val randomInt = Random.nextInt(1, 11)
// Tulostaa esim. 7

// Generoidaan satunnainen desimaaliluku väliltä 0-1
val randomDouble = Random.nextDouble()
// Tulostaa esim. 0.527

// Generoidaan satunnainen boolean arvo
val randomBoolean = Random.nextBoolean()
// Tulostaa joko true tai false
```
## Syvempi sukellus
Satunnaislukujen generointi perustuu matemaattisiin algoritmeihin, jotka tuottavat sattumanvaraisia numeroita. `Random` luokka käyttää Java:n sisäistä `java.util.Random` luokkaa, joka puolestaan käyttää XORShift algoritmia satunnaislukujen generoimiseen.

Kotlinissa on myös mahdollista käyttää `Random` luokan laajennusmetodeja, kuten `random()` tai `randomOrNull()`, jotka toimivat kuten `nextInt()` mutta mahdollistavat erilaisten tyyppien generoimisen.

## Katso myös
- [Kotlinin Random dokumentaatio](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.random/-random/index.html)
- [Java:n Random dokumentaatio](https://docs.oracle.com/javase/8/docs/api/java/util/Random.html)
- [XORShift algoritmin selitys](https://en.wikipedia.org/wiki/Xorshift)