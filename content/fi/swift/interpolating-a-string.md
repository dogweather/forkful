---
title:                "Merkkijonon interpolointi"
html_title:           "Bash: Merkkijonon interpolointi"
simple_title:         "Merkkijonon interpolointi"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/swift/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?

Merkkijonojen interpolointi Swift-ohjelmoinnissa tarkoittaa muuttujan tai arvon liittämistä suoraan merkkijonoon. Se tekee koodista selvempää ja luettavampaa, vähentäen virheiden mahdollisuutta.

## Kuinka:

Swiftilla merkkijonoihin voi lisätä muuttujia tai arvoja suoraan käyttämällä kenoviivoja ja sulkeita \(). Katso alla olevaa esimerkkiä:

```Swift
let hedelma = "omena"
let kpl = 5
let lause = "Mina syon \(kpl) \(hedelma)a paivassa."
print(lause)
```

Tuotos:

```Swift
"Mina syon 5 omenaa paivassa."
```

## Syvä sukellus

Merkkijonon interpolointi Swiftissa tuli iOS 9:n myötä, joka julkaisiin vuonna 2015. Aikaisemmissa versioissa oli käytettävä "+" -operaattorilla, joka oli sekavaa ja kömpelöä. Muissa ohjelmointikielissä, kuten Pythonissa tai Javassa, on vastaavia mekanismeja, mutta ne voivat vaihdella syntaxiltaan.

Swiftin merkkijonon interpolointi on tehokas ja joustava. Voit esimerkiksi kutsua funktioita tai tehdä laskentoja suoraan sulkeiden sisällä:

```Swift
let hinta = 2.5
let lause = "Yhdella omenalla maksaa \(hinta * Double(kpl)) euroa."
print(lause)
```

Tuotos:

```Swift
"Yhdella omenalla maksaa 12.5 euroa."
```

## Katso myös

Lisätietoja Swift-merkkijonojen interpoloinnista voit lukea seuraavista lähteistä:
- [Apple's Swift Documentation: String Interpolation](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html#ID292)
- [Swift String Interpolation - Hacking with Swift](https://www.hackingwithswift.com/example-code/strings/how-to-use-string-interpolation-to-make-custom-strings)