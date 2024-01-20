---
title:                "Päivämäärän jäsentäminen merkkijonosta"
html_title:           "Javascript: Päivämäärän jäsentäminen merkkijonosta"
simple_title:         "Päivämäärän jäsentäminen merkkijonosta"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/swift/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

# Parsing Päivämääriä Stringistä Swiftissä

## Mikä & Miksi?

Päivämäärien jäsennys (parsing) stringistä on prosessi, jossa päivämäärät muunnetaan tekstistä käyttökelpoiseen muotoon. Tätä tehdään, jotta voidaan käsittellä ja analysoida päivämääriä ohjelmissamme helposti.

## Näin teet:

Ohessa esimerkkikoodi Swiftistä, joka jäsentelee stringin päivämääräksi.

```Swift
import Foundation

let dateFormatter = DateFormatter()
dateFormatter.dateFormat = "dd.MM.yyyy"

if let date = dateFormatter.date(from: "27.12.2021") {
    print(date)
} else {
   print("Päivämäärää ei voitu muuntaa.")
}
```

Esimerkkikoodin tuloste näyttää seuraavalta:

```Swift
2021-12-27 00:00:00 +0000
```

## Syvempi Sukellus

Historiallisesti päivämääräjäsennys on ollut osa ohjelmointia lähes sen alusta asti. Vaikka se näyttää yksinkertaiselta, se voi olla yllättävän haastavaa, koska päivämäärämuotoilut vaihtelevat paljon eri puolilla maailmaa. 

Vaihtoehtoisia tapoja päivämäärien jäsennykseen Swiftissä ovat mm. `ISO8601DateFormatter` tai muokatun `DateComponents`-olion käyttö.

Kun jäsennetään päivämäärä stringistä Swiftillä, käytetään taustalla `NSDateFormatter`-luokkaa, joka on osa Foundation-paketin tarjoamia ominaisuuksia. Tämä luokka käyttää Kansainvälisen standardointijärjestön ISO-koodauksia, mukaan lukien aikavyöhyke- ja kalenterikäsittely.

## Katso myös:

Seuraavista lähteistä löydät lisätietoa päivämääräjäsennyksestä Swiftissä:

- Swiftin virallinen käsikirja: [Date ja DateFormatter](https://developer.apple.com/documentation/foundation/date)
- Stack Overflow: [Keskustelua päivämäärien jäsennyksestä Swiftissä](https://stackoverflow.com/questions/35700281/date-format-in-swift)