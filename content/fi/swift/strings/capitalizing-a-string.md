---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:06:38.132051-07:00
description: "Merkkijonon muuttaminen isokirjaimiseksi Swiftiss\xE4 muokkaa annettua\
  \ merkkijonoa siten, ett\xE4 sen ensimm\xE4inen merkki on isokirjain ja loput merkit\
  \ ovat\u2026"
lastmod: '2024-03-13T22:44:56.890961-06:00'
model: gpt-4-0125-preview
summary: "Merkkijonon muuttaminen isokirjaimiseksi Swiftiss\xE4 muokkaa annettua merkkijonoa\
  \ siten, ett\xE4 sen ensimm\xE4inen merkki on isokirjain ja loput merkit ovat\u2026"
title: Merkkijonon muuttaminen isoiksi kirjaimiksi
---

{{< edit_this_page >}}

## Mikä & Miksi?

Merkkijonon muuttaminen isokirjaimiseksi Swiftissä muokkaa annettua merkkijonoa siten, että sen ensimmäinen merkki on isokirjain ja loput merkit ovat pieniä kirjaimia. Ohjelmoijat tekevät näin esimerkiksi nimien tai lauseiden muotoilun vuoksi noudattaen kieliopillisia sääntöjä tai käyttöliittymästandardeja.

## Kuinka:

Swiftin `String`-rakenteet sisältävät muutamia sisäänrakennettuja metodeja merkkijonojen kirjainkoon manipuloimiseksi. Tässä on muutama lähestymistapa merkkijonojen isokirjaimiseksi muuttamiseen Swiftissä, mukaan lukien standardimetodien käyttö ja tarvittaessa kolmannen osapuolen kirjastojen käyttö.

### Käyttäen sisäänrakennettuja metodeja

Muuttaaksesi merkkijonon ensimmäisen kirjaimen isoksi ja loput pieniksi:

```swift
let myString = "hello, world"
let capitalizedString = myString.prefix(1).uppercased() + myString.dropFirst().lowercased()
print(capitalizedString) // Tuloste: "Hello, world"
```

Muuttaaksesi jokaisen sanan ensimmäisen kirjaimen isoksi lauseessa, voit käyttää `capitalized`-ominaisuutta:

```swift
let sentence = "hello, world"
let capitalizedSentence = sentence.capitalized
print(capitalizedSentence) // Tuloste: "Hello, World"
```

### Käyttäen kolmannen osapuolen kirjastoa

Vaikka Swiftin standardikirjasto on melko kattava, jotkut tietynlaiset isokirjainmuodot saattavat vaatia monimutkaisempia toimenpiteitä tai niitä voidaan yksinkertaistaa käyttämällä kolmannen osapuolen kirjastoja. Yksi suosittu merkkijonojen käsittelyyn tarkoitettu kirjasto on SwiftRichString. (Huom: Varmista, että sisällytät kolmannen osapuolen kirjastot Swift Package Managerin, CocoaPodsin tai Carthagen kautta, ja tuo ne tiedostoosi.)

Ensiksi sinun tulisi lisätä `SwiftRichString` projektiisi. Asennettuasi voit käyttää sitä suorittamaan erilaisia merkkijono-operaatioita, mukaan lukien tietyt isokirjainmuotoilutarpeet. Kuitenkin tällä hetkellä Swiftin sisäänrakennetut metodit kattavat suurimman osan isokirjainmuotoilutarpeista ilman, että tarvitsee käyttää ulkoisia kirjastoja pelkästään merkkijonojen isokirjaimiseksi muuttamiseen.

Viittaa aina kirjaston viimeisimpään dokumentaatioon mahdollisista päivityksistä tai muutoksista metodeissa.
