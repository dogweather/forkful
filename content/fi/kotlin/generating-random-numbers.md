---
title:                "Satunnaislukujen generointi"
date:                  2024-01-20T17:50:10.085612-07:00
model:                 gpt-4-1106-preview
simple_title:         "Satunnaislukujen generointi"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/kotlin/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why? (Mitä & Miksi?)
Satunnaislukujen generointi on prosessi, jossa luodaan ennalta-arvaamattomia numeroita. Ohjelmoijat käyttävät näitä numeroita monissa tilanteissa, kuten pelien, simulaatioiden ja turvallisuuden algoritmeissa, tuomaan vaihtelevuutta ja arvaamattomuutta.

## How to (Kuinka tehdä):
Kotlin mahdollistaa satunnaislukujen generoinnin suoraan standardikirjastossa. Tässä on muutamia esimerkkejä:

```Kotlin
import kotlin.random.Random

fun main() {
    // Luodaan satunnainen kokonaisluku väliltä 0-99
    val randomInt = Random.nextInt(100)
    println(randomInt)

    // Luodaan satunnainen liukuluku väliltä 0.0-1.0
    val randomDouble = Random.nextDouble()
    println(randomDouble)

    // Luodaan satunnainen boolean
    val randomBoolean = Random.nextBoolean()
    println(randomBoolean)
}
```
Esimerkki tulosteet voivat näyttää jotakuinkin tältä, mutta muuttuvat joka ajokerralla:
```
42
0.8124481367827148
true
```

## Deep Dive (Sukellus syvemmälle):
Satunnaislukujen generointi on vanha käsite, ja se on ollut osa ohjelmointia yhtä kauan kuin tietokoneet ovat olleet olemassa. Järjestelmien tarve satunnaisuuteen on johtanut useiden erilaisten algoritmien kehittämiseen.

Historiallisesti yksi suosituista menetelmistä on ollut lineaarisen kongruenssin generaattori (LCG), mutta modernissa ohjelmoinnissa käytetään usein monimutkaisempia algoritmeja. Kotlin käyttää `java.util.Random`-luokkaa, joka itse asiassa on pseudosatunnaislukugeneraattori, mikä tarkoittaa, että se luo numerojärjestystä, joka näyttää satunnaiselta, mutta on todellisuudessa ennustettava, kun generaattorin tila on tiedossa.

Kotlinin `Random`-luokka on sen sijaan suunniteltu tarjoamaan yksinkertaisempi ja thread-safe rajapinta satunnaislukujen generointiin. Se mahdollistaa myös helpon tavalla satunnaisen siemenarvon (seed) asettamisen, mikä on hyödyllistä, kun tarvitset toistettavaa satunnaisuutta, esimerkiksi testauksessa.

Vaihtoehtoja `Random`-luokalle ovat esimerkiksi kolmannen osapuolen kirjastot tai algoritmit, kuten Mersenne Twister, joka tunnetaan korkeasta laadustaan ja pitkästä jaksonpituudestaan.

## See Also (Lisätietoja):
- Kotlinin virallinen dokumentaatio satunnaislukujen generoinnista: [Random - Kotlin Programming Language](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.random/-random/)
- Wikipedia-artikkeli pseudosatunnaislukugeneraattoreista: [Pseudorandom number generator - Wikipedia](https://en.wikipedia.org/wiki/Pseudorandom_number_generator)
- Tietoa Mersenne Twister -algoritmistä: [Mersenne Twister - Wikipedia](https://en.wikipedia.org/wiki/Mersenne_Twister)
