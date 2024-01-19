---
title:                "Nykyisen päivämäärän hankkiminen"
html_title:           "Haskell: Nykyisen päivämäärän hankkiminen"
simple_title:         "Nykyisen päivämäärän hankkiminen"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/swift/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?
Hankitaan päivämäärätieto tietokoneen sisäisestä kellosta Swift-ohjelmointikielellä. Tätä tietoa voidaan käyttää esimerkiksi sen varmistamiseen, että tiedot tallennetaan oikealla aikaleimalla.

## Näin teet:
Alla oleva Swift-koodi hakee nykyisen päivämäärän ja tulostaa sen kehykseen.

``` Swift 
import Foundation

let nyt = Date()
print(nyt)
```
Tämä tuottaa seuraavanlaisen tuloksen:

``` Swift 
// Tulostaa: "2022-02-24 10:45:32 +0000"
```

## Syvempi tarkastelu
Historiallisesti ajantasaisen päivämäärätiedon saaminen on ollut olennainen osa ohjelmointia esimerkiksi logitietojen generoinnissa.

Vaihtoehtoisesti voimme muotoilla tulostetun päivämäärän haluamaamme muotoon, esimerkiksi "24 Feb 2022", käyttämällä DateFormatter-luokkaa seuraavasti:
``` Swift
let muotoilija = DateFormatter()
muotoilija.dateStyle = .medium
print(muotoilija.string(from: nyt))
```

Tämä saa aikaan:

```Swift
// Tulostaa: "24 Feb 2022"
```

Date- ja DateFormatter-luokat perustuvat Apple Foundation -perustan asianmukaisiin toteutuksiin, joten ne tarjoavat luotettavan lähestymistavan päivämäärätietojen hallintaan Swiftissä.

## Katso myös:
- [Apple: Working With Date and Time](https://developer.apple.com/documentation/foundation/date)
- [Swift Documentation: Date](https://developer.apple.com/documentation/foundation/date)
- [Swift Documentation: DateFormatter](https://developer.apple.com/documentation/foundation/dateformatter)