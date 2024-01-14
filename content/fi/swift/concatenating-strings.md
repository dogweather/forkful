---
title:    "Swift: Yhdistettävien merkkijonojen yhdistäminen"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/swift/concatenating-strings.md"
---

{{< edit_this_page >}}

## Miksi

Miksi haluat yhdistää merkkijonoja Swift-koodissasi? Yhdistäminen antaa sinulle mahdollisuuden luoda uusia merkkijonoja, jotka koostuvat useista erillisistä merkkijonoista. Tämä voi olla hyödyllistä esimerkiksi silloin, kun haluat luoda selkeitä ja hyvin muotoiltuja tekstirivejä tulostamiseen tai tallentamiseen.

## Miten tehdä se

Yhdistetään ensin kaksi merkkijonoa käyttämällä " + " operaattoria:

```Swift
let etunimi = "Matti"
let sukunimi = "Meikäläinen"
let kokoNimi = etunimi + " " + sukunimi
```

Tulosteena saat "Matti Meikäläinen", joka on yhdistelmä etu- ja sukunimestä. Voit myös yhdistää useita merkkijonoja yksinkertaisesti lisäämällä ne " + " merkkien väliin:

```Swift
let kaupunki = "Helsinki"
let osoite = "Mannerheimintie"
let postiosoite = kaupunki + ", " + osoite + " 21"
```

Tulosteena saat "Helsinki, Mannerheimintie 21", joka on yhdistelmä kaupungista, osoitteesta ja numerosta.

Voit myös yhdistää merkkijonoja käyttämällä yhdistetyn sijoitusoperaattorin " += " avulla:

```Swift
var lause = "Tänään on "
lause += "kaunis "
lause += "sää."
```

Tulosteena saat "Tänään on kaunis sää.", joka on yhdistelmä kolmesta erillisestä merkkijonosta.

## Syventävä tieto

Swift tarjoaa myös muita tapoja yhdistää merkkijonoja, kuten käyttämällä alilausekkeita tai yhdistämällä muuttujan tai vakion merkkijonon kanssa. Voit myös käyttää merkkijonan muotoilua käyttämällä interpolointia eli lisäämällä muuttujan tai vakion arvon merkkijonoon sen sijaan, että sekoittaisit sen " + " operaattoreilla.

Voit myös yhdistää muita tietotyyppejä, kuten numeroita ja booleaneja, merkkijonoon käyttämällä niiden tiedoston muotoilua.

## Katso myös

- [Swiftin merkkijonon perusteet](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
- [Merkkijonon muotoilu Swiftissä](https://www.hackingwithswift.com/articles/162/how-to-format-strings-in-swift-using-string-interpolation)
- [Muita vinkkejä Swift-kielen käyttöön](https://cocoacasts.com/40-essential-swift-tips-and-tricks)