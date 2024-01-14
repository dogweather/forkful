---
title:                "Kotlin: Merkkijonon pituuden löytäminen"
simple_title:         "Merkkijonon pituuden löytäminen"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/kotlin/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

# Miksi etsiä merkkijonon pituutta?

Merkkijonon pituuden etsiminen on yksi yleisimmistä ohjelmointitehtävistä, ja sillä on useita käyttötarkoituksia. Jotkut esimerkit voivat olla merkkijonojen validointi, tietokannan hakujen rajaaminen tai merkkijonojen muokkaaminen erilaisiin tarkoituksiin. Tässä blogikirjoituksessa keskitymme oppimaan, kuinka voit löytää merkkijonon pituuden Kotlin-ohjelmointikielellä.

## Miten löytää merkkijonon pituus Kotlinilla?

```kotlin
val merkkijono = "Tervetuloa blogiin"
println("Merkkijonon pituus on: ${merkkijono.length}")
```
Tässä esimerkissä käytämme merkkijonon `length`-ominaisuutta, joka palauttaa merkkijonon pituuden. Tulosteena näemme "Merkkijonon pituus on: 20", koska kyseisessä merkkijonossa on 20 merkkiä. On tärkeää huomata, että merkkijonon pituusmittari alkaa aina nollasta.

Toinen tapa löytää merkkijonon pituus on käyttää `count()`-funktiota:

```kotlin
val merkkijono = "Tervetuloa blogiin"
println("Merkkijonon pituus on: ${merkkijono.count()}")
```

Yllä olevassa esimerkissä `count()`-funktio laskee kaikki merkit merkkijonossa ja palauttaa niiden lukumäärän.

## Syväluotaus

Kuten aikaisemmin mainitsimme, merkkijonon pituuden löytäminen on yleinen tehtävä ohjelmoinnissa. Yleensä ohjelmoijana sinun tulee muistaa, että merkkijonot ovat muuttumattomia, eli niitä ei voi muokata. Siksi, aina kun teet muutoksia merkkijonoihin, luot todellisuudessa uuden merkkijonon. Tämä voi aiheuttaa haasteita muistinhallinnassa ja tehokkuudessa, jos käsittelet suuria määriä dataa.

On myös tärkeää muistaa eri kieliversioiden vaikutus merkkijonon pituuteen. Tietyt kirjaimet ja merkit voivat olla eri pituisia eri kieliversioissa, mikä voi vaikuttaa mittaukseen. Esimerkiksi ääkköset voivat olla kaksi merkkiä pitkiä joissain kieliversioissa ja yksi merkki pitkiä toisissa.

## Katso myös

- [Kotlinin virallinen dokumentaatio merkkijonojen käsittelystä](https://kotlinlang.org/docs/reference/strings.html)
- [Stack Overflow -vastaus merkkijonon pituudesta Kotlinilla](https://stackoverflow.com/questions/39680310/get-the-length-of-a-string-in-kotlin/39680372)