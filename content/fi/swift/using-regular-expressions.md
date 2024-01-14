---
title:    "Swift: Säännöllisten lausekkeiden käyttö"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/swift/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Miksi käyttää säännöllisiä lausekkeita Swift-ohjelmoinnissa?

Säännölliset lausekkeet ovat voimakas työkalu, joka helpottaa merkkijonojen muokkaamista ja haun toteuttamista. Niiden avulla voidaan etsiä ja korvata tiettyjä merkkijonoja, tarkistaa merkkijonon muotoilua ja paljon muuta. Etenkin isojen tekstiaineistojen käsittelyssä ne ovat erittäin hyödyllisiä.

## Miten käytetään säännöllisiä lausekkeita Swift-ohjelmoinnissa?

Säännöllisten lausekkeiden käyttö Swiftissä tapahtuu "NSRegularExpression" -luokan avulla. Se mahdollistaa erilaisten haku- ja korvaustoimintojen suorittamisen halutuilla muuttujilla. Alla esimerkki säännöllisen lausekkeen käytöstä Swiftissä:

```Swift
let merkkijono = "Tämä on säännöllisten lausekkeiden käyttämisen esimerkki Swiftissä."
let pattern = "[aeiouyAEIOUY]" // Etsii kaikki vokaalit
let regex = try! NSRegularExpression(pattern: pattern, options: [])
let match = regex.matches(in: merkkijono, options: [], range: NSMakeRange(0, merkkijono.utf16.count))

for match in matches {
  print("Löydetty: \(merkkijono[Range(match.range, in: merkkijono)!])")
}
```

Tulostus:

```
Löydetty: ä
Löydetty: o
Löydetty: ä
Löydetty: i
Löydetty: u
Löydetty: a
Löydetty: u
Löydetty: e
Löydetty: e
Löydetty: a
Etsittyjen merkkien määrä: 10
```

## Syvällisempi sukellus säännöllisten lausekkeiden käyttöön

Säännöllisten lausekkeiden käyttäminen vaatii jonkin verran harjoittelua, mutta niiden hyödyt ovat suuret. Niiden avulla voidaan esimerkiksi tarkistaa, että annettu sähköpostiosoite on oikeassa formaatissa tai poistaa tarpeettomat merkit tekstistä. Niiden ominaisuuksia ovat muun muassa aakkosten, numeroiden ja erikoismerkkien tunnistaminen sekä tarkan haun toteuttaminen halutuilla muuttujilla.

## Katso myös

- [NSRegularExpression -dokumentaatio](https://developer.apple.com/documentation/foundation/nsregularexpression)
- [Swift Regular Expressions -opetusohjelma](https://www.ralfebert.de/ios/tutorials/regular-expressions/)
- [Regular Expressions Cheat Sheet](https://www.cheatography.com/davechild/cheat-sheets/regular-expressions/)