---
title:                "Tulevaisuuden tai menneen päivämäärän laskeminen"
html_title:           "Kotlin: Tulevaisuuden tai menneen päivämäärän laskeminen"
simple_title:         "Tulevaisuuden tai menneen päivämäärän laskeminen"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/kotlin/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Mitä ja miksi?
Päivämäärien laskeminen tulevaisuudessa tai menneisyydessä on yleinen tehtävä ohjelmoinnissa. Tämä voidaan tehdä esimerkiksi tilanteissa, joissa halutaan tarkistaa, kuinka monta päivää on jäljellä tiettyyn tapahtumaan tai laskea tulevan päivämäärän perusteella tulevien päivien sää. Ohjelmoijat tekevät tämän lisäämällä tai vähentämällä päiviä nykyisestä päivästä.

## Näin teet sen:
### Esimerkki 1:
```Kotlin
val currentDate = LocalDate.now() // nykyinen päivämäärä
val futureDate = currentDate.plusDays(30) // lisätään 30 päivää nykyiseen päivään
println(futureDate) // tulostaa tulevan päivämäärän 30 päivää nykyisestä päivästä
```
Tulostus:
```
2021-08-19
```
### Esimerkki 2:
```Kotlin
val inputDate = "2021-12-25" // syötetty päivämäärä
val christmas = LocalDate.parse(inputDate) // päivämäärän muuntaminen
val currentDate = LocalDate.now() // nykyinen päivämäärä
val daysUntilChristmas = ChronoUnit.DAYS.between(currentDate, christmas) // päivien välisen eron laskeminen
println(daysUntilChristmas) // tulostaa päivien määrän jäljellä jouluun
```
Tulostus:
```
131
```

## Syvemmälle:
Päivämäärien laskeminen ei ole uusi asia ohjelmoinnissa. Jo 1700-luvulla matemaatikot ja astronoomit kehittelivät erilaisia kaavoja päivämäärien laskemiseen. Nykyään eri ohjelmointikielillä on valmiita kirjastoja, jotka helpottavat päivämäärien käsittelyä. Jos haluat tehdä päivämäärien laskemisen monimutkaisemmaksi, voit esimerkiksi ottaa huomioon karkausvuodet tai eri aikavyöhykkeet.

## Katso myös:
- [LocalDate - Kotlin](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-local-date/index.html)
- [Date and Time API - Oracle](https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html)