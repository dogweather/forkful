---
title:                "Päivämäärän laskeminen tulevaisuudessa tai menneisyydessä"
html_title:           "Kotlin: Päivämäärän laskeminen tulevaisuudessa tai menneisyydessä"
simple_title:         "Päivämäärän laskeminen tulevaisuudessa tai menneisyydessä"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/kotlin/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Miksi

Päivämäärien laskeminen tulevaisuuteen ja menneisyyteen voi olla hyödyllistä esimerkiksi tapahtumien suunnittelussa tai raportoinnissa.

## Miten tehdä

Päivämäärien laskeminen tulevaisuuteen tai menneisyyteen onnistuu helposti Kotlin-ohjelmointikielellä. Voit käyttää tähän tarkoitukseen `Calendar`-luokkaa, joka löytyy `java.util`-paketista. Seuraavassa esimerkissä lasketaan päivämäärä yhden viikon päähän käyttäen `Calendar`-luokkaa.

```Kotlin
val calendar = Calendar.getInstance()
calendar.add(Calendar.DAY_OF_YEAR, 7)
val date = calendar.time
println(date)
```
**Tulostus:**

`Sat Sep 25 21:13:15 EDT 2021`

Voit myös asettaa päivämäärän tiettyyn muotoon käyttämällä `SimpleDateFormat`-luokkaa. Tässä esimerkissä päivämäärä muotoillaan `dd/MM/yyyy`-muotoon.

```Kotlin
val sdf = SimpleDateFormat("dd/MM/yyyy")
val formattedDate = sdf.format(date)
println(formattedDate)
```
**Tulostus:**

`25/09/2021`

## Syvemmälle aiheeseen

`Calendar`-luokan lisäksi Kotlinissa on käytettävissä myös `LocalDateTime`-luokka, joka helpottaa päivämäärien käsittelyä ja muokkaamista. `LocalDateTime`-luokka tarjoaa mahdollisuuden esimerkiksi eri aikavyöhykkeiden huomioimiseen ja päivämäärien vertailuun.

```Kotlin
val date1 = LocalDateTime.now()
val date2 = date1.plusDays(14)
println(date1.isBefore(date2)) //tulostaa true
```

## Katso myös

- [Kotlinin virallinen dokumentaatio](https://kotlinlang.org/docs/home.html)
- [Java Calendar-luokan käyttöohjeet](https://docs.oracle.com/javase/8/docs/api/java/util/Calendar.html)
- [Java LocalDateTime-luokan käyttöohjeet](https://docs.oracle.com/javase/8/docs/api/java/time/LocalDateTime.html)