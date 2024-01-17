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

## Mikä ja miksi?
Kotlin-ohjelmointikielessä nykyisen päivämäärän hankkiminen tarkoittaa nykyisen päivämäärän ja ajan näyttämistä tietokoneellesi. Ohjelmoijat tekevät tämän yleensä seuratakseen ajan kulkua ja tallentaakseen päivämäärän ja ajan tietokannassa.

## Kuinka?
Käytä seuraavaa koodia hankkiaksesi nykyisen päivämäärän ja ajan Kotlinissa:

```Kotlin
import java.time.LocalDate

val currentDate = LocalDate.now()

println(currentDate)
```

Tämän koodin tulostus voi olla esimerkiksi: "2021-11-12".

## Syväsukellus
Hankkimalla nykyisen päivämäärän ja ajan tietokoneesi kautta, voit tarkkailla ajan kulkua ja seurata esimerkiksi tapahtumien ajoitusta. Tämä voi myös auttaa tallentamaan päivämäärän ja ajan esimerkiksi tietokantaan tai tekemään aikaleimauksia.

On olemassa myös muita tapoja hankkia nykyinen päivämäärä ja aika Kotlinissa, kuten käyttämällä Java Calendar- tai LocalDateTime-luokkia. Jokaisella lähestymistavalla voi olla omat etunsa ja haittansa.

## Katso myös
Voit lukea lisää päivämäärän ja ajan hankkimisesta Kotlinilla esimerkiksi Kotlinin dokumentaatioista tai Stack Overflow -sivustolta: 
https://kotlinlang.org/docs/datetime.html
https://stackoverflow.com/questions/39182501/how-to-get-current-date-in-kotlin