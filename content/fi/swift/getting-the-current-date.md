---
title:                "Lain päivämäärän hankkiminen"
html_title:           "Swift: Lain päivämäärän hankkiminen"
simple_title:         "Lain päivämäärän hankkiminen"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/swift/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?

Päivämäärän hankkiminen tarkoittaa nykyisen päivämäärän - eli tämän päivän päivämäärän - saamista tietokoneessa. Ohjelmoijat tekevät tämän usein, koska he tarvitsevat tietoa ajasta ohjelmassaan tai sen ulkopuolella.

## Miten:

Hanki nykyinen päivämäärä Swiftillä käyttämällä Date-luokan nykyistä metodia. Alla olevassa koodiesimerkissä tulostamme nykyisen päivämäärän konsoliin.

```Swift
let currentDate = Date() 
print(currentDate) // tulostaa esimerkiksi "2019-10-02 12:34:56 +0000"
```

Voit myös muokata tulostuksen formaattia käyttämällä DateFormatter-luokkaa ja sen apufunktioita. Esimerkiksi alla oleva koodi tulostaa päivämäärän muodossa "2.10.2019".

```Swift
let currentDate = Date() 
let dateFormatter = DateFormatter()
dateFormatter.dateFormat = "dd.MM.yyyy"
let formattedDate = dateFormatter.string(from: currentDate)
print(formattedDate) // tulostaa "2.10.2019"
```

## Syvä sukellus:

Päivämäärän hankkiminen on tärkeä osa ohjelmointia, sillä se liittyy lähes kaikkiin ohjelmiin jollakin tavalla. Alunperin päivämäärän hankkiminen oli monimutkaisempaa, mutta nykyään Swift tarjoaa helpon tavan käsitellä päivämääriä Date-luokan avulla. On myös muita tapoja hankkia päivämäärä, kuten käyttämällä alustariippumattomia kirjastoja kuten Foundation tai Cocoa.

## Katso myös:

- [Apple Swift - Dokumentaatio](https://developer.apple.com/documentation/swift/date)
- [Foundation-luokka päivämäärän hankkimiseen](https://developer.apple.com/documentation/foundation/nsdate)
- [NSCalendar-luokka ajan laskemiseen ohjelmassa](https://developer.apple.com/documentation/foundation/nscalendar)