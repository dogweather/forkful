---
title:    "Swift: Tulostus virheenjäljityslähtöön"
keywords: ["Swift"]
---

{{< edit_this_page >}}

## Miksi

Miksi tulostamme debuggaustietoja ohjelmassamme?

Tulostamalla debuggaustietoja voimme tarkastella ohjelmamme käyttäytymistä ja löytää mahdollisia virheitä ja bugeja. Näiden tietojen avulla voimme myös seurata ohjelman suoritusta ja optimoida sen suorituskykyä. Debuggaustietojen tulostaminen on siis erittäin tärkeää ohjelmistokehityksen kannalta.

## Miten

Alla on esimerkki siitä, miten voimme tulostaa debuggaustietoja Swift-koodissa:

```Swift
let name = "Maija"
let age = 25

print("Käyttäjän nimi on \(name) ja ikä on \(age) vuotta.")

```

Tämä koodi tulostaa seuraavan viestin:

<code> Käyttäjän nimi on Maija ja ikä on 25 vuotta. </code>

Voit myös tulostaa arvojen lisäksi esimerkiksi muuttujan nimen tai jonkin ehtolauseen arvon, jotta voit seurata ohjelman suoritusta tarkemmin. Esimerkiksi:

```Swift
let number = 6
if number % 2 == 0 {
    print("\(number) on parillinen.")
} else {
    print("\(number) on pariton.")
}
```

Tämä koodi tulostaisi:

<code> 6 on parillinen.</code>

Voit myös käyttää "debugPrint" -funktiota, joka antaa enemmän tietoa tulostettavasta arvosta. Esimerkiksi:

```Swift
debugPrint(number)
```

Tämä tulostaisi:

<code> Int(6) </code>

## Syvemmälle

Debuggaustietojen tulostaminen voi joskus hidastaa ohjelman suoritusta. Tästä syystä on tärkeää muistaa poistaa debuggaustulostukset lopullisesta koodista. Voit tehdä tämän helpoiten lisäämällä "#if DEBUG" -ehdon ennen tulostuskomentoa ja "#endif" sen jälkeen. Tällöin tulostus tapahtuu vain silloin, kun ohjelma suoritetaan debug-tilassa, eikä vaikuta lopulliseen suorituskykyyn.

## Katso myös

- Debuggaus Swiftissä: https://developer.apple.com/documentation/swift/debugging
- Debuggausvinkkejä Swiftissä: https://www.raywenderlich.com/416-the-ios-8-swift-debugger-part-1-2
- Debuggaustietojen analysointi Xcode-ohjelmassa: https://www.iphonelife.com/content/top-10-xcode-debugging-tips-tips-and-tricks-ios-developers