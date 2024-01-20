---
title:                "Päivämäärän muuttaminen merkkijonoksi"
html_title:           "Go: Päivämäärän muuttaminen merkkijonoksi"
simple_title:         "Päivämäärän muuttaminen merkkijonoksi"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/kotlin/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?

Date String -muunnos on prosessi, jossa päivämäärä muunnetaan merkkijonoksi. Tämä auttaa paremmassa datakäsittelyssä ja ihmiskäyttäjille sopivan esitystavan tarjoamisessa.

## Kuinka toimii:

Kotlinin 'SimpleDateFormat'-luokan avulla voidaan muuntaa päivämäärä merkkijonoksi näin:

```Kotlin
import java.text.SimpleDateFormat
import java.util.Date

fun main(args: Array<String>) {
    val date = Date()
    val sdf = SimpleDateFormat("dd/M/yyyy")
    val dateString = sdf.format(date)
    println("Päivämäärä merkkijonona: $dateString")
}
```

Suoritettaessa, esimerkiksi:

```
Päivämäärä merkkijonona: 28/12/2022
```

## Syvä sukellus:

1) Historiallinen konteksti: Alun perin Javan 'SimpleDateFormat'-luokka tuotiin käytettäväksi Kotlinissa saattamaan helppokäyttöisen ja joustavan päivämäärän muotoilun ja parsinnan.
2) Vaihtoehtoja: Jos haluat muotoilla päivämäärän merkkijonoksi eri tavalla, voit muokata 'SimpleDateFormat'-mallia. Esimerkiksi: "dd MMMM yyyy HH:mm:ss".
3) Toteutuksen yksityiskohdat: Muotoilussa täytyy olla varovainen, koska väärät mallit voivat tuottaa odottamattomia tuloksia tai virheitä.

## Katso myös:

1) Kotlinin virallinen dokumentaatio: https://kotlinlang.org/docs/what-is-kotlin.html
2) SimpleDateFormat-luokan Javadoc: https://docs.oracle.com/javase/7/docs/api/java/text/SimpleDateFormat.html
3) Kotlinin päivämäärien ja aikojen käsittely: https://kotlinlang.org/docs/dates-and-times.html