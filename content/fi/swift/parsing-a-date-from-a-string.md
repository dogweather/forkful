---
title:                "Ajan erottaminen merkkijonosta"
html_title:           "Swift: Ajan erottaminen merkkijonosta"
simple_title:         "Ajan erottaminen merkkijonosta"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/swift/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

Mitä & Miksi?

Päivämäärän lukeminen merkkijonosta tarkoittaa, että koodi pystyy ottamaan merkkijonon ja muuntamaan sen päivämääränä. Tämä on erittäin hyödyllistä, kun tarvitsemme tarkkoja päivämääriä tietokannoissa tai muissa järjestelmissä. Ohjelmoijat tekevät tämän helpottaakseen päivämäärän käsittelyä ja tarjotakseen tarkempia tietoja.

Kuinka:

Esimerkkejä koodista ja tulosteesta löydät alla olevista Swift-koodilohkoista:

```Swift
// Luo muuttuja, joka sisältää merkkijonon päivän 6. toukokuuta vuonna 2021
let dateStr = "6.5.2021"

// Muunna merkkijono päivämääräksi ja tulosta se konsoliin
let formatter = DateFormatter()
formatter.dateFormat = "dd.MM.yyyy"
if let date = formatter.date(from: dateStr) {
    print("Tulostus: \(date)") // Tulostus: 2021-05-06 00:00:00 +0000
}
```

Tämä koodi luo ensin muuttujan, joka sisältää merkkijonon päivän 6. toukokuuta vuonna 2021. Sitten se muuntaa tämän merkkijonon päivämääräksi käyttämällä DateFormatter-luokkaa ja päivämäärämäärämuotoilijaa. Lopuksi se tulostaa päivämäärän konsoliin.

Syväsukellus:

Jos haluat tarkempaa tietoa päivämäärien käsittelystä Swiftissä, voit tutustua seuraaviin lähteisiin:

- Historiallinen tausta: Päivämäärien käsittely on ollut haasteellista koodaajille pitkään ja on yhä tärkeä osa monia järjestelmiä.
- Vaihtoehtoiset tavat: Voit myös käyttää Foundation Frameworksia tai AppKit/Cocoa -kirjastoa käsitelläksesi päivämääriä Swiftissä.
- Toteutuksen yksityiskohdat: Jos haluat tietää lisää siitä, kuinka DateFormatter-muotoilija toimii taustalla, voit lukea dokumentaatiosta.

Katso myös:

Jos haluat lisätietoja päivämäärän lukemisesta merkkijonosta Swiftissä, voit tarkistaa seuraavat lähteet:

- Swiftin virallinen dokumentaatio: Sieltä löydät lisätietoja Date-muodosta, DateFormatter-luokasta ja päivämäärän muotoilusta Swiftissä.
- Stack Overflow: Siellä on paljon hyödyllisiä vastauksia ja esimerkkejä päivämäärän käsittelystä Swiftissä.
- RayWenderlich-sivusto: Se tarjoaa monia opetusohjelmia ja vihjeitä päivämäärien käsittelystä Swiftissä.