---
title:                "Merkkien poistaminen vastaavalla mallilla"
html_title:           "Arduino: Merkkien poistaminen vastaavalla mallilla"
simple_title:         "Merkkien poistaminen vastaavalla mallilla"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/swift/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

# Miten ja miksi poistaa merkkejä vastaamalla kuvioon Swiftissä

## Mikä & Miksi?

Merkkijonojen käsittelyyn kuuluu usein hahmon (kuten tietyt merkit tai hahmot) poisto. Tämän tekeminen auttaa ohjelmoijia parantamaan datan puhtautta tai muotoilemaan sen halutulla tavalla.

## Miten tehdä:

Tässä on esimerkki siitä, kuinka voit poistaa kaikki numerot merkkijonosta Swiftissä.

```Swift
var str: String = "Swift123Programming456"

let strippedStr = str.filter { !"0123456789".contains($0) }

print(strippedStr)  // Tulostaa: "SwiftProgramming"
```

## Syvä sukellus:

On hyödyllistä ymmärtää, miksi ja milloin näitä tekniikoita kehitettiin. Usein ne liittyvät ohjelmistosuunnittelun historian teemoihin, kuten tehokkuuden ja luettavuuden parantamiseen.

### Historia

Hahmojen poistaminen on peräisin vanhoista koodikielistä. Sen avulla voitiin käsitellä dataa tehokkaammin, koska se säästi muistia ja vähensi laskentaa.

### Vaihtoehdot

Swiftissä voit myös käyttää `replacingOccurrences(of:with:)` -metodia, jos haluat korvata tietyn merkkijonon toisella.

```Swift
var esimerkkiString = "Hello, World!123"
esimerkkiString = esimerkkiString.replacingOccurrences(of: "123", with: "")
print(esimerkkiString)  // Tulostaa: "Hello, World!"
```

### Toteutuksen tiedot

`filter(_:)` -funktio Swiftissä suodattaa elementtejä kollektiosta tietyn ehdon mukaan. Tässä tapauksessa ehdon (`!"0123456789".contains($0)`) on tosi, kun merkki ei kuulu lueteltuihin numeroihin.

## Katso myös:

Lisätietoja ja resursseja aiheesta löydät seuraavista lähteistä:

- [Apple Developer Documentation: String](https://developer.apple.com/documentation/swift/string)
- [Stack Overflow: How to remove specific characters from a string in Swift?](https://stackoverflow.com/questions/26797739/how-to-remove-specific-characters-from-a-string-in-swift)