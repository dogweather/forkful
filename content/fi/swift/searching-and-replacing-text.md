---
title:                "Swift: Tekstin etsiminen ja korvaaminen"
simple_title:         "Tekstin etsiminen ja korvaaminen"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/swift/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Miksi: 
Tekstin etsimisen ja korvaamisen prosessi on tärkeä osa ohjelmoinnissa, sillä se auttaa tehokkaasti muokkaamaan ja päivittämään koodia.

## Kuinka: 
Koodin sisältämän tekstin etsiminen ja korvaaminen Swift-ohjelmoinnissa on helppoa ja nopeaa käyttämällä sisäänrakennettua "replacingOccurrences(of: "old text", with: "new text")" -funktiota. Alla on esimerkkejä:

```Swift
// Etsi ja korvaa yksittäinen sana
var teksti = "Tervetuloa Suomeen!"
teksti = teksti.replacingOccurrences(of: "Suomeen", with: "Ruotsiin")
// teksti nyt: "Tervetuloa Ruotsiin!"

// Etsi ja korvaa useampi sana
var uudetSanat = ["mahtava", "hienoa", "upea"]
teksti = teksti.replacingOccurrences(of: "Tervetuloa", with: uudetSanat.joined(separator: " "))
// teksti nyt: "mahtava hienoa upea Ruotsiin!"
```

## Syvempi sukellus:
Tekstin etsimisen ja korvaamisen prosessi voidaan hallita tarkemmin käyttämällä "range(of:options:range:locale:)" -funktiota yhdessä "replacingCharacters(in: "replaceRange", with: "new text")" -funktion kanssa. Tämä mahdollistaa tarkan sijainnin ja tyyppisten sanojen etsimisen ja korvaamisen.

Esimerkiksi, jos haluat korvata vain ensimmäisen esiintymän tietyistä sanoista tekstissä, voit käyttää seuraavaa koodia:

```Swift
var teksti = "Tämä on esimerkkiteksti, jossa on useita sanoja"
if let alkupaikka = teksti.range(of: "esimerkki") {
    teksti.replaceSubrange(alkupaikka, with: "uusi")
}
// teksti nyt: "Tämä on uusiteksti, jossa on useita sanoja"
```

## Katso myös:
- [Apple Developer Documentation: String](https://developer.apple.com/documentation/swift/string)
- [Hacking with Swift: How to replace a substring with another substring](https://www.hackingwithswift.com/example-code/strings/how-to-replace-a-substring-with-another-substring)
- [Swift Education: Searching and Replacing Text in Swift Strings](http://swiftpedia.ru/utf-8/functions/rangeofoptionsrangelocale/)