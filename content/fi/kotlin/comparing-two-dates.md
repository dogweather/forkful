---
title:                "Kahden päivämäärän vertaaminen"
html_title:           "Bash: Kahden päivämäärän vertaaminen"
simple_title:         "Kahden päivämäärän vertaaminen"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/kotlin/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Mitä ja Miksi?
Vertaamme kahta päivämäärää, kun haluamme tietää, kumpi päivämäärä sijoittuu aikajanalla ennen tai jälkeen. Ohjelmoijat tekevät niin esimerkiksi aikasarja-analyysissä ja loki-data-analysoinnissa.

## Kuinka tehä:
Tässä on esimerkki, kuinka voit verrata kahta päivämäärää Kotlin-koodissa.

```Kotlin
import java.time.LocalDate

fun main() {
    val pvm1 = LocalDate.of(2020, 1, 1)
    val pvm2 = LocalDate.of(2021, 1, 1)
    
    if (pvm1.isBefore(pvm2)) {
        println("pvm1 on ennen pvm2")
    } else if (pvm1.isAfter(pvm2)) {
        println("pvm1 on jälkeen pvm2")
    } else {
        println("pvm1 ja pvm2 ovat samat")
    }
}
```
Tässä otoksen tuloksessa, `pvm1` on ennen `pvm2`, joten tulostettu viesti on "pvm1 on ennen pvm2".

## Syventyminen
Päivämäärien vertailu ei ole uusi konsepti ohjelmoinnissa. Sen taustalla on paljon monimutkaisuutta, jota ei päivittäisessä ohjelmointikäytössä aina huomioi. Päivämäärän ja ajan vertailussa huomioitavia tekijöitä voivat olla aikavyöhykkeet, karkausvuodet ja DST (Daylight Saving Time).

Vaihtoehtoina Kotlinin sisäänrakennetulle funktiolle on monia kirjastoja, kuten Joda-Time. Jotkut näistä tarjoavat lisää joustavuutta ja yksityiskohtaista tarkastelua monimutkaisissa tilanteissa.

Päivämäärien vertailun toteuttamiseen Kotlinissa ei tarvitse kirjoittaa monimutkaista funktiota. Kotlinin sisäänrakennettu LocalDate-luokka käsittelee päivämäärän vertailun kaikki yksityiskohdat.

## Katso myös
Jos haluat tietää lisää, tutustu näihin lähteisiin:

Kotlinin LocalDate-dokumentointi: https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.js/-date/index.html

Joda-Time-dokumentointi: http://joda-time.sourceforge.net/api-release/index.html