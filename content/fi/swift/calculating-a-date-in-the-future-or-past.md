---
title:                "Tulevan tai menneen päivämäärän laskeminen tietokoneohjelmoinnissa"
html_title:           "Swift: Tulevan tai menneen päivämäärän laskeminen tietokoneohjelmoinnissa"
simple_title:         "Tulevan tai menneen päivämäärän laskeminen tietokoneohjelmoinnissa"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/swift/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?

Päivämäärän laskeminen tulevaisuuteen tai menneisyyteen tarkoittaa päivämäärän lisäämistä tai vähentämistä tiettyyn ajanjaksoon nykyisestä päivämäärästä. Ohjelmoijat tekevät tätä tarpeen mukaan esimerkiksi laskemalla työpäivien määrän tulevaa tapahtumaa varten tai laskemalla paljonko aikaa on kulunut jostain tietystä päivämäärästä.

## Kuinka:

```Swift
let currentDate = Date()

// Lisätään 10 päivää nykyiseen päivämäärään
let futureDate = Calendar.current.date(byAdding: .day, value: 10, to: currentDate)

// Vähennetään 5 viikkoa nykyisestä päivämäärästä
let pastDate = Calendar.current.date(byAdding: .weekOfMonth, value: -5, to: currentDate)

// Tulostetaan tuleva ja mennyt päivämäärä
print("Tuleva päivämäärä: \(futureDate)")
print("Menenyt päivämäärä: \(pastDate)")
```

**Tulos:**

```
Tuleva päivämäärä: Optional(2020-12-24 01:44:02 +0000)
Menneyt päivämäärä: Optional(2020-11-14 01:44:02 +0000)
```

## Syväsukellus:

Päivämäärän laskemisen tarve on ollut olemassa jo pitkään ja siihen on olemassa useita erilaisia ratkaisuja ohjelmointikielistä riippuen. Swiftissä päivämäärän laskemiseen käytetään Calendar-luokkaa, jonka avulla voi lisätä tai vähentää päiviä, viikkoja, kuukausia tai vuosia nykyiseen päivämäärään. Lisäksi on olemassa muitakin käteviä työkaluja kuten DateComponents ja DateFormatter.

## Katso myös:

- [Swiftin virallinen dokumentaatio päivämäärän laskemisesta](https://developer.apple.com/documentation/foundation/calendar)
- [Stack Overflow viisi tapaa lisätä päiviä nykyiseen päivämäärään Swiftillä](https://stackoverflow.com/questions/34075675/add-days-to-nsdate-date-in-swift)
- [Swiftille tehty kirjasto päivämäärän laskemista varten](https://github.com/malcommac/SwiftDate)