---
title:                "Nykyisen päivämäärän saaminen"
html_title:           "Swift: Nykyisen päivämäärän saaminen"
simple_title:         "Nykyisen päivämäärän saaminen"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/swift/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Miksi

Monissa ohjelmoinnin tehtävissä on tarpeen käyttää nykyistä päivämäärää, kuten esimerkiksi tapahtumien seurannassa tai aikaleimojen lisäämisessä tietokantoihin. Swiftillä on helppo saada tämä tieto käyttöön ja tässä artikkelissa näytämme, miten se tehdään.

## Kuinka

```Swift

let currentDate = Date() // Luo uuden Date-olion, joka sisältää nykyisen päivämäärän.

let dateFormatter = DateFormatter() // Alustetaan DateFormatter-olio, jolla muotoillaan päivämäärä.

dateFormatter.dateFormat = "dd.MM.yyyy" // Asetetaan haluttu päivämäärän muoto.

let formattedDate = dateFormatter.string(from: currentDate) // Muotoillaan nykyinen päivämäärä ja tallennetaan se muuttujaan.

print(formattedDate) // Tulostaa esimerkiksi "17.09.2021".

```

## Syvällinen sukellus

Date-luokka Swiftissä tarjoaa monia hyödyllisiä metodeita ja ominaisuuksia päivämäärän käsittelyyn. Esimerkiksi voit käyttää DateComponents-oliota eri osien, kuten vuoden, kuukauden ja päivän, lukemiseen tai asettamiseen suoraan. Voit myös käyttää Date-metodia addingTimeInterval sekuntien lisäämiseen tai vähentämiseen nykyisestä päivämäärästä.

## Katso myös

- [Date-luokan dokumentaatio Swiftissä](https://developer.apple.com/documentation/foundation/date)
- [DateFormatterin dokumentaatio Swiftissä](https://developer.apple.com/documentation/foundation/dateformatter)
- [DateComponentsin dokumentaatio Swiftissä](https://developer.apple.com/documentation/foundation/datecomponents)