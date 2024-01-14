---
title:                "Kotlin: Päivämäärän muuttaminen merkkijonoksi"
programming_language: "Kotlin"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/kotlin/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Miksi

Kotlin-ohjelmointikieli tarjoaa monia hyödyllisiä ominaisuuksia, jotka tekevät siitä suositun kehittäjien keskuudessa. Yksi näistä ominaisuuksista on kyky muuttaa päivämäärä merkkijonoksi. Tämä on erittäin hyödyllinen toiminto monissa ohjelmointitehtävissä, kuten tietokannoissa tai käyttäjän syötteiden muokkaamisessa.

## Kuinka tehdä se

Tämän toiminnon suorittamiseksi tarvitsemme `DateFormat`-luokkaa, joka on osa Kotlinin `java.text`-kirjastoa. Tämä luokka tarjoaa useita eri toimintoja, jotka mahdollistavat päivämäärän muuntamisen merkkijonoksi sekä halutun muotoilun säätämisen.

```
Kotlin 
// Luodaan uusi DateFormat-instanssi
val dateFormat = DateFormat.getDateInstance(DateFormat.LONG, Locale.getDefault())

// Määritellään haluttu päivämäärä
val calendar = Calendar.getInstance()
calendar.set(Calendar.DAY_OF_MONTH, 12)
calendar.set(Calendar.MONTH, 3)
calendar.set(Calendar.YEAR, 2020)

// Muunnetaan päivämäärä merkkijonoksi ja tulostetaan se
val dateString = dateFormat.format(calendar.time)
println(dateString)

// Tulos: 12. huhtikuuta 2020
```

## Syvällisempi katsaus

`DateFormat`-luokan `getDateInstance()`-metodi mahdollistaa päivämäärän ja ajan muuttamisen halutulla tavalla. Voimme muuttaa päivämäärän ja ajan muotoa, kieltä ja jopa alueellisia asetuksia. Voimme myös käyttää muita `DateFormat`-metodeja, kuten `getDateTimeInstance()` tai `getTimeInstance()` päivämäärän ja ajan yhdistämiseksi tai vain ajan muuntamiseen.

On myös tärkeää huomata, että `DateFormat`-luokka käyttää Java-kirjastoa, joten voimme käyttää kaikkia Java-koodin ominaisuuksia, kuten `Locale.getDefault()`, jotta saamme käyttäjän alueellisen sijainnin oletusarvona.

## Katso myös

- [Java 8: n uudet päivämäärät ja kello API: t](https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html)
- [Kotlin language reference](https://kotlinlang.org/docs/reference/)