---
title:                "Tulevan tai menneen päivämäärän laskeminen"
html_title:           "Kotlin: Tulevan tai menneen päivämäärän laskeminen"
simple_title:         "Tulevan tai menneen päivämäärän laskeminen"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/kotlin/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?
Lasketaan tulevaisuuden tai menneisyyden päivämäärät, eli selvitämme uuden päivämäärän lisäämällä tai vähentämällä päiviä, viikkoja, kuukausia tai vuosia tietystä päivämäärästä. Ohjelmoijat tekevät tämän usein esimerkiksi aikaleimojen käsittelyssä tai sovellusten aikaan liittyvissä toiminnoissa.

## Kuinka:
Kotlinilla voit käyttää java.time.LocalDate-luokan metodeja 'plusDays', 'minusDays', 'plusMonths', 'minusMonths', 'plusYears' ja 'minusYears'. Huomaa, että metodit palauttavat uuden LocalDate-olion, eivät muuta nykyistä oliota.

```Kotlin
import java.time.LocalDate

fun main() {
    var date = LocalDate.of(2021, 1, 1)
    println("Alkuperäinen päivämäärä: $date")
  
    date = date.plusDays(15)
    println("15 päivää myöhemmin: $date")
  
    date = date.minusMonths(2)
    println("2 kuukautta aiemmin: $date")
  
    date = date.plusYears(5)
    println("5 vuotta myöhemmin: $date")
}
```

## Sukellus syvyyksiin:
Historiaan meneminen, 'java.util.Date'- ja 'java.util.Calendar'-luokat tarjosivat aiemmin nämä ominaisuudet Javassa, mutta ne olivat sekavia ja bugisia. Niiden tilalle tuli java.time-paketti Javassa 8.

Vaihtoehtona on käyttää kirjastoa, kuten Joda-Time, jos sinun on työskenneltävä Javan versioissa, jotka eivät sisällä java.time-pakettia.

Päivämäärän laskeminen tulevaisuudessa tai menneisyydessä on pohjimmiltaan suhteellisen aikayksikön lisäämistä tai vähentämistä. Kotlin / Java tekevät tämän sisäisesti päivän millisekunteina.

## Katso myös:
- [Kotlinin virallinen dokumentaatio päivämäärien ja ajan käsittelystä](https://kotlinlang.org/docs/dates-periods-in-standard-library.html)
- [Java 8 Date Time API](https://www.oracle.com/technical-resources/articles/java/jf14-date-time.html)
- [Joda-Time -kirjasto](https://www.joda.org/joda-time/)