---
title:                "Päivämäärän muuttaminen merkkijonoksi."
html_title:           "Kotlin: Päivämäärän muuttaminen merkkijonoksi."
simple_title:         "Päivämäärän muuttaminen merkkijonoksi."
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/kotlin/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Miksi

Miksi haluaisit muuttaa päivämäärän merkkijonoksi? Se voi olla hyödyllistä esimerkiksi kun haluat tallentaa päivämäärän tietokantaan tai näyttää sen käyttäjälle selkeässä muodossa.

## Miten

Kotlinin avulla voit helposti muuttaa päivämäärän merkkijonoksi. Se käyttää Java Date and Time API:ta, joten voit käyttää samoja metodeja kuin Javassa.

Esimerkiksi jos haluat muuttaa päivämäärän nykyisestä ajankohdasta merkkijonoksi, voit käyttää seuraavaa koodia:

```Kotlin
import java.time.LocalDate
import java.time.format.DateTimeFormatter

val currentDate = LocalDate.now()
val formattedDate = currentDate.format(DateTimeFormatter.ofPattern("dd.MM.yyyy"))

println("Päivämäärä merkkijonona: $formattedDate")
```

Tämä tulostaisi esimerkiksi "20.09.2020" riippuen nykyisestä päivämäärästä.

Voit myös muuttaa merkkijonon takaisin päivämääräksi käyttämällä `LocalDate.parse()` -metodia. Esimerkiksi:

```Kotlin
val dateAsString = "20.09.2020"
val parsedDate = LocalDate.parse(dateAsString, DateTimeFormatter.ofPattern("dd.MM.yyyy"))

println("Merkkijono muutettuna päivämääräksi: $parsedDate")
```

Tämä tulostaisi "2020-09-20".

## Syvempi sukellus

Kotlinin Java Date and Time API tarjoaa paljon erilaisia mahdollisuuksia muuttaa päivämäärä merkkijonoksi ja päinvastoin. Voit käyttää erilaisia formaatteja pukeaksesi päivämäärän haluamaasi muotoon. Esimerkiksi:

- "d" - päivämäärä ilman nollia (esim. "5")
- "dd" - päivämäärä kaksinumeroisena (esim. "05")
- "MMM" - kuukauden lyhennetty nimi (esim. "Sep")
- "MMMM" - kuukauden koko nimi (esim. "Syyskuu")
- "y" - vuosi ilman vuosisadan lukua (esim. "20")
- "yy" - vuosi kaksinumeroisena (esim. "20")
- "yyyy" - vuosi nelinumeroisena (esim. "2020")

Voit löytää lisää erilaisia formaatteja [Kotlinin dokumentaatiosta](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-date-time/index.html#constants).

## Katso myös

- [Java 8 Date and Time API](https://www.baeldung.com/java-8-date-time-intro)
- [Kotlinin virallinen dokumentaatio](https://kotlinlang.org/docs/reference/datetime.html)