---
title:                "Nykyisen päivämäärän saaminen"
html_title:           "Kotlin: Nykyisen päivämäärän saaminen"
simple_title:         "Nykyisen päivämäärän saaminen"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/kotlin/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Miksi

Koska päivämäärän saaminen on tärkeä osa lähes jokaisen ohjelman toimintaa. Se voi auttaa meitä esimerkiksi näyttämään päivämäärän käyttöliittymässä tai käyttämään sitä muissa laskelmissa.

## Kuinka

```Kotlin 
import java.time.LocalDate

// Päivämäärän saaminen nykyisestä ajasta
val nykyinenPaivamaara = LocalDate.now() 

// Päivämäärän saaminen tietystä aikaleimasta
val haluttuPaivamaara = LocalDate.of(2020, 5, 15) 

// Päivämäärän muotoilu halutunlaiseksi
val muotoiltuPaivamaara = haluttuPaivamaara.format(DateTimeFormatter.ofPattern("dd.MM.yyyy")) 

// Päivämäärän vertailu toiseen päivämäärään
if (nykyinenPaivamaara.isAfter(haluttuPaivamaara)) {
    println("Nykyinen päivämäärä on myöhemmin kuin haluttu päivämäärä.")
} else {
    println("Haluttu päivämäärä on myöhemmin kuin nykyinen päivämäärä.")
}
```

**Tulostus:** Haluttu päivämäärä on myöhemmin kuin nykyinen päivämäärä.

## Syventävä sukellus

Nykyisen päivämäärän saaminen kotlinissa on helppoa ja kätevää verrattuna vanhempiin ohjelmointikieliin. Tämä johtuu siitä, että Kotlin käyttää Java 8:ssa esiteltyä uutta aikapakkausta: java.time. Tämä pakkaus tarjoaa monia hyödyllisiä metodeja ja luokkia, joita voidaan käyttää päivämäärän käsittelyssä.

Kotlin tarjoaa myös mahdollisuuden käyttää haluttua päivämäärän muotoilua käyttämällä DateTimeFormatter-luokkaa, joka helpottaa päivämäärän esittämistä halutussa muodossa. Tämä voi olla erityisen hyödyllistä, jos päivämäärän esitysmuoto on tärkeä tietyssä ohjelmassa.

## Katso myös

- [Kotlinin virallinen verkkosivusto] (https://kotlinlang.org/)
- [Java Time API-opas] (https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html)
- [Kotlinin päivämäärä- ja aikatoiminnot] (https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.time/index.html)