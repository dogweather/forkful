---
title:                "Swift: Päivämäärän muuttaminen merkkijonoksi."
programming_language: "Swift"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/swift/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Miksi

Monissa ohjelmointiprojekteissa on tarve muuttaa päivämäärä merkkijonoksi, jotta se voidaan esittää käyttäjille tai tallentaa tietokantaan. Tässä blogikirjoituksessa käymme läpi, miten tämä onnistuu Swift-ohjelmointikielellä.

## Miten

Päivämäärän muuttaminen merkkijonoksi onnistuu helposti Swiftillä. Tässä esimerkissä luomme ensin päivämäärä-muuttujan ja määritämme sille tietyn päivämäärän.

```Swift
let date = Date()
```

Seuraavaksi käytämme DateFormatter-luokkaa määrittämään, millaisessa muodossa haluamme näyttää päivämäärän.

```Swift
let dateFormatter = DateFormatter()
dateFormatter.dateFormat = "yyyy-MM-dd"
```

Lopuksi kutsumme DateFormatterin string(from:) -metodia, joka palauttaa päivämäärän merkkijonona halutussa muodossa.

```Swift
let dateString = dateFormatter.string(from: date)
print(dateString)
// Output: 2021-04-04
```

## Syvempi sukellus

DateFormatter-luokka tarjoaa monia erilaisia vaihtoehtoja päivämäärän ja ajan muuttamiseen haluttuun muotoon. Voit esimerkiksi lisätä tunnit, minuutit ja sekunnit päivämäärään antamalla dateFormat-muuttujalle seuraavanlaisen arvon: "yyyy-MM-dd HH:mm:ss". Voit myös muuttaa päivämäärän ja ajan merkkijonoksi yhdellä kertaa antamalla seuraavanlaisen arvon dateFormat-muuttujalle: "yyyy-MM-dd HH:mm:ss Z".

DateFormatter-luokan lisäksi Swiftillä on mahdollista käyttää myös DateComponentsFormatter-luokkaa, jolla voi muuttaa esimerkiksi ajan sekunneiksi, minuuteiksi ja tunneiksi.

## Katso myös

- [DateFormatter-luokan dokumentaatio Swiftin virallisilla verkkosivuilla](https://developer.apple.com/documentation/foundation/dateformatter)
- [DateFormatter-tutoriaali Ray Wenderlichin verkkosivuilla](https://www.raywenderlich.com/1481-nsdateformatter-tutorial-for-beginners-with-swift)

Kiitos lukemisesta ja onnea päivämäärän muuttamiseen haluamaasi muotoon Swiftillä!