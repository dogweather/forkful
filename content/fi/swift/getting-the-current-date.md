---
title:    "Swift: Nykyisen päivämäärän saaminen"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/swift/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Miksi

Oletko koskaan tarvinnut tietää nykyisen päivämäärän ja ajan? Esimerkiksi laskutusta tai aikaleimoja varten? Swift-ohjelmointikieli tarjoaa helpon tavan hakea nykyinen päivämäärä ja aika, ja tässä blogikirjoituksessa opit, miten se tehdään.

## Miten

Saadaksesi nykyisen päivämäärän Swiftillä, seuraavat koodirivit riittävät:

```Swift
let currentDate = Date()
let dateFormatter = DateFormatter()
dateFormatter.dateFormat = "dd.MM.yyyy HH:mm:ss"
let currentDateString = dateFormatter.string(from: currentDate)
print(currentDateString)
```

Ensimmäisellä rivillä luodaan muuttuja `currentDate`, joka sisältää nykyisen päivämäärän ja ajan. Toisella rivillä luodaan DateFormatter-olio, joka auttaa muotoilemaan halutunlaisen merkkijonon päivämäärästä. Seuraavalla rivillä määritellään haluttu muotoilu käyttämällä `dateFormat`-ominaisuutta. Lopuksi `string(from:)`-metodilla muodostetaan halutun muotoinen merkkijono `currentDate`-muuttujasta ja tulostetaan se konsoliin.

Tässä esimerkissä päivämäärästä muodostetaan merkkijono muodossa "päivä.kuukausi.vuosi tunti:minuutti:sekunti", mutta käytännössä voit muotoilla sen haluamallasi tavalla. Esimerkiksi kirjoittamalla `dateFormatter.dateFormat = "HH:mm"` saat tulostettua vain nykyisen ajan muodossa "tunti:minuutti".

## Syvemmälle

Nykyisen päivämäärän saaminen Swiftillä perustuu käytännössä `Date`-rakenteeseen ja `DateFormatter`-luokkaan. `Date`-rakenne sisältää tiedon nykyisestä ajankohdasta ja `DateFormatter`-luokka auttaa muotoilemaan tuon tiedon halutun muotoiseksi merkkijonoksi.

On myös hyvä huomata, että `dateFormat`-ominaisuus sekä `string(from:)`-metodi käyttävät merkkejä merkkaamaan erilaisia osia päivämäärässä, kuten päivän, kuukauden ja vuoden. Voit lukea lisää näistä merkeistä [DateFormatterin dokumentaatiosta](https://developer.apple.com/documentation/foundation/dateformatter), jossa on myös esimerkkejä erilaisista formaattivaihtoehdoista.

## Katso myös

- [Apple Developer Documentation: Date](https://developer.apple.com/documentation/foundation/date)
- [Apple Developer Documentation: DateFormatter](https://developer.apple.com/documentation/foundation/dateformatter)