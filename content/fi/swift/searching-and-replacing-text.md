---
title:                "Tekstin etsiminen ja korvaaminen"
html_title:           "Swift: Tekstin etsiminen ja korvaaminen"
simple_title:         "Tekstin etsiminen ja korvaaminen"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/swift/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Miksi

Ehkä olet huomannut, että vakioviestisi sisältösi tarvitsee päivittämistä tai haluat vaihtaa samankaltaiset lauseet toisiksi. Ei ole tarpeellista kirjoittaa jokaisen rivin uudestaan, silloin voit käyttää tekstinhakua ja korvausta. Se on yksinkertainen tapa säästää aikaa ja varmistaa, että viestisi on yhtenäinen ja ajantasainen.

## Kuinka tehdä se

Käytä tekstinhakufunktiota ja korvaa siihen haluamasi lause. Voit myös käyttää säännöllisiä lausekkeita tarkentamaan hakua. Tutustu esimerkkituloksiin alla olevissa Swift-koodilohkoissa.

```Swift
// Alustetaan vakioviesti
var viesti = "Tervetuloa!"

// Hakua ja korvausta käyttämällä muutetaan viesti
viesti = viesti.replacingOccurrences(of: "Tervetuloa", with: "Hei")

// Tulostetaan muokattu viesti
print(viesti)

// Tulostaa: Hei!
```

```Swift
// Toinen esimerkki
var lause = "Pysy terveenä ja turvassa!"

// Korvataan vain osa lauseesta
lause = lause.replacingOccurrences(of: "turvassa", with: "onnellisena")

// Tulostetaan muokattu lause
print(lause)

// Tulostaa: Pysy terveenä ja onnellisena!
```

## Syvemmälle aiheeseen

Swiftin `replacingOccurrences(of:with:)` -metodilla voidaan korvata ei ainoastaan tekstiä, vaan myös merkkijonoja, vaihtaa osia merkkijonosta ja paljon muuta. Voit myös käyttää erilaisia säännöllisiä lausekkeita hakujen tarkentamiseen, kuten `[aeiou]` korvaamaan kaikki vokaalit haluamallasi merkillä. Älä epäröi kokeilla erilaisia vaihtoehtoja löytääksesi parhaan ratkaisun tarpeisiisi.

## Katso myös

- [Apple Developer Documentation: Text Searching and Replacement](https://developer.apple.com/documentation/foundation/nsstring/1417171-replacingoccurrences)
- [Regular Expressions Tutorial in Swift](https://www.raywenderlich.com/86205/nsregularexpression-swift-tutorial)
- [Swift Regular Expression Cheat Sheet](https://www.swiftbysundell.com/posts/regular-expressions-in-swift)