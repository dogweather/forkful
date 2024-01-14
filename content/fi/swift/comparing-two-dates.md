---
title:    "Swift: Vertaillaan kahta päivämäärää"
keywords: ["Swift"]
---

{{< edit_this_page >}}

## Miksi

Usein ohjelmoinnin yhteydessä joudutaan vertailemaan kahta päivämäärää, esimerkiksi tarkistaessa onko tietty päivä tulevaisuudessa tai menneisyydessä. Tässä blogipostauksessa käsittelemme, miten voit vertailla kahta päivämäärää käyttämällä Swift-ohjelmointikieltä.

## Miten

Yksi tapa vertailla päivämääriä Swiftissä on käyttää ```Date```-luokan ```compare```-metodia. Tämä metodi palauttaa ```ComparisonResult```-tyypin arvon, joka kertoo, onko ensimmäinen päivämäärä ennen, jälkeen vai sama kuin toinen päivämäärä. Esimerkiksi:

```Swift
let dateFormatter = DateFormatter()
dateFormatter.dateFormat = "yyyy-MM-dd"
let ensimmäinenPäivä = dateFormatter.date(from: "2021-05-01")
let toinenPäivä = dateFormatter.date(from: "2021-06-01")

switch ensimmäinenPäivä.compare(toinenPäivä) {
case .orderedAscending:
    print("Ensimmäinen päivä on ennen toista päivää")
case .orderedDescending:
    print("Ensimmäinen päivä on jälkeen toista päivää")
case .orderedSame:
    print("Päivämäärät ovat samat")
}
```

Tämä esimerkki tulostaa: ```Ensimmäinen päivä on ennen toista päivää```.

## Syvemmälle

Päivämäärien vertailu voi olla monimutkaisempaa kuin pelkän päivämäärän tarkastelun suhteen. Esimerkiksi jos haluat verrata aikaa, jolloin kaksi tapahtumaa tapahtuivat, sinun on otettava huomioon myös tunti- ja minuuttilisäykset. Voit tehdä tämän käyttämällä saman ```DateFormatter```-luokan ```timeStyle```-ominaisuutta. Esimerkiksi:

```Swift
let dateFormatter = DateFormatter()
dateFormatter.dateStyle = .none
dateFormatter.timeStyle = .medium
let ensimmäinenTapahtuma = dateFormatter.date(from: "2021-05-01 15:30:00")
let toinenTapahtuma = dateFormatter.date(from: "2021-05-01 18:45:00")

if let ensimmäinen = ensimmäinenTapahtuma, let toinen = toinenTapahtuma {
    if ensimmäinen.compare(toinen) == .orderedAscending {
        print("Ensimmäinen tapahtuma tapahtui ennen toista")
    } else {
        print("Ensimmäinen tapahtuma tapahtui jälkeen toista")
    }
}
```

Tämä esimerkki tulostaa: ```Ensimmäinen tapahtuma tapahtui ennen toista```.

## Katso myös

- [Swiftin virallinen dokumentaatio](https://docs.swift.org/swift-book/LanguageGuide/ClassesAndStructures.html)
- [Hacking with Swift - Päivämäärien vertailu](https://www.hackingwithswift.com/example-code/language/how-to-compare-dates-in-swift)
- [Codecademy - Päivämäärien vertailu Swiftissä](https://www.codecademy.com/courses/learn-swift/lessons/your-second-swift-program/exercises/date-comparison)