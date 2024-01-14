---
title:    "Swift: Tavanomaisen virheen kirjoittaminen"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/swift/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Miksi kirjoittaisin standardivirheeseen?

Kirjoittaminen standardivirheeseen on tehokas tapa hallita ohjelmien tuottamaa tietoa ja virheilmoituksia. Se auttaa kehittäjiä selvittämään ongelmia ja korjaamaan koodissa olevia virheitä.

## Miten kirjoitat standardivirheeseen?

Voit kirjoittaa standardivirheeseen käyttämällä Swiftin "print()" -funktiota ja lisäämällä siihen "FileHandle.standardError" -parametrin. Tämä ohjaa tulosteen standardivirheeseen sen sijaan, että se tulostettaisiin konsoliin.

```
Swift
print("Tämä on virheilmoitus", to: FileHandle.standardError)
```
Tämän koodin suorittamisen jälkeen näet virheilmoituksen standardivirheessä sen sijaan, että se tulostuisi konsoliin.

## Syväkatsaus: Tietoa kirjoittamisesta standardivirheeseen

Kun kirjoitat standardivirheeseen, se auttaa sinua tunnistamaan ja korjaamaan ohjelman virheitä. Tämä johtuu siitä, että ohjelmat käsittelevät standardivirhettä eri tavalla kuin konsoliin tulostettavaa tietoa. Lisäksi voit määrittää erilaisia virheettömiä tapauksia, jolloin virheilmoitusta ei tulosteta standardivirheeseen.

Standardivirheeseen kirjoittamisen hyödyntäminen auttaa myös kehittäjiä ohjelmien kehityksessä, koska sitä voidaan käyttää myös muutosten jäljittämiseen ja testaamiseen.

## Katso myös

- [Swiftin dokumentaatio standardivirheestä](https://developer.apple.com/documentation/swift/filehandle/standarderror)
- [Ohjeet virheiden käsittelystä Swiftissä](https://www.swiftbysundell.com/basics/error-handling/)
- [Keskusteluohjelma-aihe virheiden käsittelystä Swiftissä](https://forums.swift.org/t/behind-the-swift-error-handling-model/2221)