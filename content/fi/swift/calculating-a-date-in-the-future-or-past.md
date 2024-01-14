---
title:    "Swift: Tulevien tai menneiden päivämäärien laskeminen tietokoneohjelmoinnissa"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/swift/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Miksi

Usein meillä on tarve laskea tietty päivämäärä tulevaisuudessa tai menneisyydessä, esimerkiksi tapahtumaa tai lomamatkaa suunnitellessamme. Onneksi Swift tarjoaa helpon tavan laskea päivämäärä tulevaisuudessa tai menneisyydessä olevan annetun päivän perusteella.

## Miten

Lasketaan ensin tuleva päivämäärä käyttäen `DateComponents` ja `Calendar` rakenteita. Näitä rakenteita voidaan käyttää määrittelemään päivämäärä komponenttien kuten vuosi, kuukausi ja päivä perusteella. Esimerkiksi, jos haluat laskea päivämäärän 10 päivää tulevaisuudessa, voit käyttää seuraavaa koodia:

```Swift
let calendar = Calendar.current
var dateComponents = DateComponents()
dateComponents.day = 10
let futureDate = calendar.date(byAdding: dateComponents, to: Date())!
```

Tämä koodi lisää 10 päivää nykyiseen päivämäärään ja tallentaa sen `futureDate` muuttujaan.

Vastaavasti voit myös laskea menneisyyden päivämäärän käyttämällä negatiivista arvoa `dateComponents.day` komponentissa.

```Swift
dateComponents.day = -5
let pastDate = calendar.date(byAdding: dateComponents, to: Date())!
```

Tämä koodi vähentää 5 päivää nykyisestä päivämäärästä ja tallentaa sen `pastDate` muuttujaan.

## Syvällisempi tutustuminen

Voit myös määrittää muita komponentteja, kuten vuotta, kuukautta tai tuntia, `dateComponents` rakenteessa ja lisätä tai vähentää niitä samalla tavalla `Calendar` rakenteen avulla.

On myös tärkeää huomata, että `dateComponents` rakenteella määritetyt päivämäärät ovat aina paikallisia, eli ne perustuvat käyttäjän nykyiseen aikavyöhykkeeseen. Voit kuitenkin asettaa eri aikavyöhykkeen `Calendar` rakenteen `timeZone` ominaisuuden avulla.

## Katso myös

- [Apple Developer Documentation: Calendar](https://developer.apple.com/documentation/foundation/calendar)
- [Apple Developer Documentation: DateComponents](https://developer.apple.com/documentation/foundation/datecomponents)
- [Swift by Sundell: Working with dates in Swift](https://www.swiftbysundell.com/basics/dates/)
- [Hacking with Swift: How to work with dates and times using DateComponents](https://www.hackingwithswift.com/articles/181/how-to-work-with-dates-and-times-using-datecomponents)