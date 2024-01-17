---
title:                "Satunnaislukujen generointi"
html_title:           "Kotlin: Satunnaislukujen generointi"
simple_title:         "Satunnaislukujen generointi"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/kotlin/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?
Satunnaisluvun generoiminen on prosessi, jossa tietokone tuottaa numeroita sattumanvaraisesti. Tämä on hyödyllistä ohjelmoinnissa esimerkiksi arpajaisten tai salasanoiden luomiseen.

## Miten:
Kotlinissa satunnaislukuja voidaan generoida käyttämällä ```Random``` -luokkaa. Ota ensin käyttöön tämä luokka ja sen funktiot ```nextInt()``` ja ```nextDouble()```, joilla voit generoida kokonaislukuja ja desimaalilukuja. Katso alla olevia koodiesimerkkejä ja tulostetuita tuloksia.

Koodi esimerkki #1:
```
import java.util.Random
fun main() {
    val random = Random()
    val randomNumber = random.nextInt()
    println("Satunnainen kokonaisluku: $randomNumber")
}
```
Tulostus:
```
Satunnainen kokonaisluku: -1525835769
```

Koodi esimerkki #2:
```
import java.util.Random
fun main() {
    val random = Random()
    val randomNumber = random.nextDouble()
    println("Satunnainen desimaaliluku: $randomNumber")
}
```
Tulostus: 
```
Satunnainen desimaaliluku: 0.2597412903505213
```


## Syväsukellus:
Satunnaislukujen generointi on ollut tärkeä osa ohjelmointia jo pitkään, sillä se on olennainen osa monia sovelluksia, kuten pelien ja satunnaislukugeneraattoreiden. Aiemmin monet ohjelmoijat käyttivät itse luotuja algoritmeja, mutta nykyään monilla ohjelmointikielillä on sisäänrakennettuja työkaluja, kuten Kotlinin ```Random``` -luokka.

## Katso myös:
- [Kotin virallinen verkkosivusto](https://kotlinlang.org/)
- [Kotin Github sivu](https://github.com/JetBrains/kotlin)
- [Kotin dokumentaatio](https://kotlinlang.org/docs/home.html)