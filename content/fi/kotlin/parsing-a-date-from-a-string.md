---
title:                "Päivämäärän jäsentäminen merkkijonosta"
html_title:           "Bash: Päivämäärän jäsentäminen merkkijonosta"
simple_title:         "Päivämäärän jäsentäminen merkkijonosta"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/kotlin/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

# Päivämäärän jäsentäminen merkkijonosta Kotlin-ohjelmointikielessä

## Mitä ja Miksi?
Merkkijonosta päivämäärän jäsentäminen tarkoittaa tekstissä olevan päivämäärän siirtämistä ymmärrettäväksi päivämäärä-objektiksi. Käytämme tätä tekniikka yleisesti tiedostojen luennassa, käyttäjän syötetietojen analysoinnissa, ja päivämäärätiedon manipuloinnissa.

## Kuinka toimin:
Jäsentäminen on yksinkertaista Kotlinissa:
```Kotlin
    import java.time.LocalDate
    import java.time.format.DateTimeFormatter

    fun main() {
        val formatter = DateTimeFormatter.ofPattern("d.MM.yyyy")
        val dateString = "23.12.2021"
        val date = LocalDate.parse(dateString, formatter)
        println(date) // Tulostaa: 2021-12-23
    }
```
Tässä ohjelmassa muunnamme merkkijonon "23.12.2021" päivämääräksi ja tulostamme päivämäärän.

## Syvä Sukellus:
Varhaisissa ohjelmointikielissä päivämäärätiedot käsiteltiin usein merkkijonoina. Nykyaikaiset ohjelmointikielet, kuten Kotlin, tarjoavat helpompia ja tehokkaampia tapoja käsittellä päivämääriä. Alternatiiveista parhaita ovat Joda-Time ja ThreeTenABP-kirjastot.

`LocalDate.parse` perustuu java.time-paketin toiminnallisuuteen. Jos haluat mukautetun muodon, voit luoda `DateTimeFormatter` -objektin ja määrittää oman muodon stringinä. Muoto "d.MM.yyyy" tarkoittaa päivää (d), kuukautta (M) ja vuotta (y). 

## Katso myös:
[Kotlinin virallinen dokumentaatio](https://kotlinlang.org/docs/dates-and-times.html)
[Joda-Time](https://www.joda.org/joda-time/)
[ThreeTenABP](https://github.com/JakeWharton/ThreeTenABP)

Huomioithan, että valinta riippuu paljon tarpeista ja tilanteestasi. Jotkut kirjastot voivat paremmin tukea erityistarpeitasi kuin toiset.