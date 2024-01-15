---
title:                "Lasketaan päivämäärä tulevaisuudessa tai menneisyydessä."
html_title:           "Swift: Lasketaan päivämäärä tulevaisuudessa tai menneisyydessä."
simple_title:         "Lasketaan päivämäärä tulevaisuudessa tai menneisyydessä."
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/swift/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Miksi

On monia syitä miksi ohjelmoijat saattavat haluta laskea tietyn päivän tulevaisuudessa tai menneisyydessä. Yksi yleinen syy on esimerkiksi sovellusten ja ohjelmien luominen, jotka tarjoavat tietoja tulevista tapahtumista tai muistuttavat käyttäjää tärkeistä päivistä.

## Kuinka

Laskeminen tietyn päivän tulevaisuudessa tai menneisyydessä ei ole monimutkaista Swiftissä. Tässä on muutama esimerkki ja niiden tulosteet, jotka näyttävät kuinka voit tehdä sen helposti käyttämällä Date-luokkaa ja Calendaria.

``` Swift
// Import Calendar package
import Foundation

// Get today's date
let currentDate = Date()

// Calculate date in future
let futureDate = Calendar.current.date(byAdding: .day, value: 5, to: currentDate)
print(futureDate)

// Output:
// "2021-10-07 11:27:27 +0000"

// Calculate date in past
let pastDate = Calendar.current.date(byAdding: .year, value: -3, to: currentDate)
print(pastDate)

// Output:
// "2018-10-02 11:27:27 +0000"

```

Jos haluat lisätä enemmän joustavuutta laskutoimituksiin, voit myös käyttää DateComponentsia ja DateFormatteria. DateComponentsia voit käyttää määrittämään tarkemmin mitä haluat lisätä tai vähentää päivämäärästä.

``` Swift
// Import Calendar package
import Foundation

// Set DateComponents for 5 days
var dateComponent = DateComponents()
dateComponent.day = 5

// Get current date
let currentDate = Date()

// Calculate date in future
let futureDate = Calendar.current.date(byAdding: dateComponent, to: currentDate)

// Format date to "dd/MM/yyyy" format
let dateFormatter = DateFormatter()
dateFormatter.dateFormat = "dd/MM/yyyy"
print(dateFormatter.string(from: futureDate!))

// Output:
// "07/10/2021"
```

## Syväsukellus

Date-luokka ja Calendar ovat erittäin hyödyllisiä työkaluja päivämäärän laskemisessa tulevaisuudessa tai menneisyydessä Swiftissä. Date-luokka edustaa tiettyä päivämäärää ja kellonlyömää, kun taas Calendar-luokka auttaa laskemaan päivämäärien välistä eroa ja tekemään muutoksia päivämäärään.

On myös hyödyllistä muistaa, että päivämäärät voivat vaihdella eri aikavyöhykkeiden välillä, joten sinun tulisi aina tarkistaa ja muuntaa päivämäärät haluttuun aikavyöhykkeeseen ennen laskemista tai näyttämistä.

## Katso myös

- [Swiftin virallinen dokumentaatio päivämäärien laskemisesta](https://developer.apple.com/documentation/foundation/date)
- [Yleiset päivämäärälaskentavirheet ja niiden ratkaiseminen Swiftissä](https://medium.com/analytics-vidhya/common-date-calculation-errors-in-swift-and-how-to-fix-them-1cc926b7f5ac)
- [Kuinka lasketaan aikaa ja päivämäärää tulevaisuudessa Swiftissä](https://www.hackingwithswift.com/example-code/system/how-to-calculate-the-difference-between-two-dates)