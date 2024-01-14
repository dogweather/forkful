---
title:    "Swift: Merkkijonon muuntaminen pieniksi kirjaimiksi"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/swift/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Miksi 

On monia syitä, miksi haluat muuttaa merkkijonon pieniksi kirjaimiksi Swift-ohjelmoinnissa. Yksi yleisimmistä syistä on johdonmukaisuus ja yhtenäisyyden säilyttäminen datassa. Esimerkiksi jos tietokannassa tai käyttäjän syötteissä on eri muodossa olevia sanoja (esimerkiksi "KISSA" ja "kissa"), muuntamalla ne pieniksi kirjaimiksi, voidaan välttää turhia eroja ja virheitä datan käsittelyssä.

## Miten

Merkinjonon muuttaminen pieniksi kirjaimiksi Swiftissä on yksinkertaista käyttämällä metodia "lowercased()". Tämä metodi palauttaa uuden merkkijonon, jossa kaikki kirjaimet ovat pieniä.

```Swift
let sana = "KISSA"
print(sana.lowercased()) // tulostaa "kissa"
```

Jos haluat muuttaa merkkijonon pieniksi kirjaimiksi alkuperäisessä muuttujassa, voit käyttää metodia "lowercased()" yhdessä sijoitusoperaattorin kanssa.

```Swift
var sana = "KISSA"
sana = sana.lowercased()
print(sana) // tulostaa "kissa"
```

Voit myös käyttää metodia suoraan kirjainketjujen käsittelyssä käyttämällä muuttujan nimeä sulkeiden sisällä.

```Swift
let tulos = "APINA".lowercased()
print(tulos) // tulostaa "apina"
```

## Syväsukellus

Kun merkkijonojen käsittelyyn tarvitaan enemmän monimutkaisuutta, Swift tarjoaa myös muita vaihtoehtoja. Esimerkiksi voit käyttää metodia "localizedLowercase()", joka muuttaa merkkijonon pieniksi kirjaimiksi, ottaen huomioon myös mahdolliset kielelliset ja alueelliset erot.

```Swift
let sana = "ÄITI"
print(sana.localizedLowercase) // tulostaa "äiti" kielestä riippuen
```

Voit myös käyttää ns. "string interpolation" -tekniikkaa, jolla voit yhdistää merkkijonoja suoraan metodin muokattavana olevaan versioon.

```Swift
let lempiväri = "SININEN"
let lause = "Minun lempivärini on \((lempiväri).lowercased())." // palauttaa "Minun lempivärini on sininen."
```

## Katso myös

- [Apple Developer Documentation - String](https://developer.apple.com/documentation/swift/string/) (englanniksi)
- [Swift By Sundell - Working With Strings in Swift](https://www.swiftbysundell.com/basics/strings/) (englanniksi)
- [Swift-tietokirja - Merkkijonot](https://docs.swift.org/swift-book/LanguageGuide/Strings.html) (englanniksi)