---
title:                "Päivämäärän muuttaminen merkkijonoksi"
html_title:           "Go: Päivämäärän muuttaminen merkkijonoksi"
simple_title:         "Päivämäärän muuttaminen merkkijonoksi"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/swift/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

# Päivämäärän muuttaminen merkkijonoksi Swiftissä

## Mikä & Miksi?
Päivämäärän muuttaminen merkkijonoksi Swiftissä on prosessi, jolla päivämäärä esitetään helposti luetavana tekstinä. Ohjelmoijat tekevät tämän toiminnon, jotta voivat esittää päivämäärän sopivassa muodossa käyttäjän tai järjestelmän tarpeiden mukaisesti.

## Näin teet:
```Swift
import Foundation

let nyt = Date()
let muotoilija = DateFormatter()
muotoilija.dateFormat = "yyyy-MM-dd HH:mm:ss"

let merkkijonoPvm = muotoilija.string(from: nyt)
print(merkkijonoPvm)
```
Tämän esimerkin tulostus olisi jotakin tämänkaltaista:`2022-04-08 12:45:10`

## Syvempi tarkastelu
Historiallisesti ottaen päivämäärän esittäminen merkkijonona on ollut osa ohjelmointia jo sen alkuaikoina. Tämä johtuu siitä, että tekstiesitys on ihmisen luontevampi tapa tulkita päivämääriä. Swiftissä `DateFormatter` -luokka tarjoaa monipuoliset työkalut päivämäärän esittämiseen merkkijonona.

Swiftin lisäksi ohjelmoijilla on myös muita vaihtoehtoja päivämäärän käsittelyyn. Esimerkiksi JavaScriptissä voidaan käyttää `Date` -objektin `toLocaleDateString` -metodia.

`DateFormatterin` käyttö mahdollistaa monipuolisen päivämäärämuotoilun. "yyyy-MM-dd HH:mm:ss" on yleisin käytetty muoto, mutta muotoilija voi määritellä tarpeen mukaan uusia päivämäärämuotoja.

## Katso myös
1. Swiftin virallinen dokumentaatio: [Date](https://developer.apple.com/documentation/foundation/date)
2. Date muuntaminen stringiksi: [Stackoverflow](https://stackoverflow.com/questions/35700281/date-format-in-swift)
3. Swiftin `Date` ja `DateFormatter`: [Ray Wenderlich](https://www.raywenderlich.com/1850-date-and-time-programming-guide-for-swift)