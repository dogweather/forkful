---
title:                "Kahden päivämäärän vertailu"
html_title:           "Kotlin: Kahden päivämäärän vertailu"
simple_title:         "Kahden päivämäärän vertailu"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/kotlin/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Miksi

Kaksi päivämäärää voi joskus olla tarpeen vertailla esimerkiksi ohjelmoinnin kontekstissa, kuten tarkasteltaessa, kumpi päivämäärä on myöhempi tai oliko tietty tapahtuma ennen vai jälkeen toisen päivämäärän. Tämä artikkeli tarjoaa yksinkertaisen ja helpon tavan vertailla kahta päivämäärää käyttäen Kotlin-ohjelmointikieltä.

## Kuinka

Vertailemalla kahta päivämäärää Kotlinissa on useita eri tapoja, riippuen siitä mitä haluat tarkalleen saada selville. Tässä esittelemme kaksi yleisintä tapaa vertailla päivämääriä ja niiden tulostusta.

```Kotlin
// Luodaan ensimmäinen päivämäärä
val date1 = LocalDate.of(2020, 11, 15)
// Luodaan toinen päivämäärä
val date2 = LocalDate.of(2020, 11, 20)

// Tapa 1: Käytetään isAfter() ja isBefore() metodeita
println(date1.isAfter(date2)) //tulostaa "false"
println(date1.isBefore(date2)) //tulostaa "true"

// Tapa 2: Käytetään compareTo() metodia, joka palauttaa 0 jos päivämäärät ovat samat,
// negatiivisen luvun jos date1 on ennen date2 ja positiivisen luvun jos date1 on date2 jälkeen
println(date1.compareTo(date2)) //tulostaa "-5"
```

Tarkemman vertailun tekemiseksi voit myös käyttää päivämäärien sisältämiä arvoja, kuten päivä, kuukausi tai vuosi, ja vertailla niitä haluamallasi tavalla.

## Syvempi sukellus

Päivämäärien vertailu perustuu niiden sisältämiin arvoihin, kuten päivään, kuukauteen ja vuoteen. Tämä tarkoittaa sitä, että vertailun lopputulos voi vaihdella eri aikavyöhykkeissä. Jos haluat suorittaa tarkemman vertailun, sinun kannattaa tutustua Java.time-paketin dokumentaatioon, joka tarjoaa kattavat työkalut päivämäärien käsittelyyn Kotlinissa.

## Katso myös

- [Kotlin Documentation](https://kotlinlang.org/docs/datetime.html)
- [Java.time Documentation](https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html) 
- [Comparing LocalDate objects in Java](https://www.geeksforgeeks.org/comparing-localdate-objects-in-java/)