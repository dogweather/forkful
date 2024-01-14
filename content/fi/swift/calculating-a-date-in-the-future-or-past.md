---
title:                "Swift: Ajan laskeminen tulevaisuudessa tai menneisyydessä"
programming_language: "Swift"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/swift/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Miksi

Laskeminen tulevaisuuden tai menneisyyden päivämääriä voi olla hyödyllistä esimerkiksi kalenterin sovelluksissa, tehtävälistoissa tai järjestelmissä, jotka seuraavat päivämääriä.

## Miten

```Swift
let date = Date() // nykyinen päivämäärä
let calendar = Calendar.current

// lasketaan tulevaisuuden päivämäärä käyttäen pvm-komponentteja
if let futureDate = calendar.date(byAdding: .day, value: 7, to: date) {
    print(futureDate) // tulostaa päivämäärän 7 päivän kuluttua nykyisestä
}

// lasketaan menneisyyden päivämäärä käyttäen pvm-komponentteja
if let pastDate = calendar.date(byAdding: .year, value: -1, to: date) {
    print(pastDate) // tulostaa päivämäärän vuosi sitten
}
```

## Syvällisempi katsaus

Päivämäärien laskeminen Swiftillä vaatii käyttöä `Date`- ja `Calendar`-luokista. Laskennassa käytetään pvm-komponentteja, kuten päivä, kuukausi ja vuosi, joita voidaan lisätä tai vähentää nykyisestä päivämäärästä. On myös mahdollista määrittää tarkempi päivämäärä, esimerkiksi "7 päivää 36 minuuttia" tulevaisuuteen tai menneisyyteen.

## Katso myös

- [Swiftin virallinen dokumentaatio pvm-komponenteista](https://developer.apple.com/documentation/foundation/calendar)
- [Kuinka käyttää päivämääriä SwiftUI:ssä](https://www.appcoda.com/swiftui-date-time/)