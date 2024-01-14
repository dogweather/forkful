---
title:                "Swift: Merkkijonon muuttaminen pieniksi kirjaimiksi"
programming_language: "Swift"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/swift/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Miksi konvertoida String alakirjaimiin?

Kirjoitettaessa ohjelmia Swift-kielellä, saattaa joskus olla tarpeellista muuttaa annettu teksti pieniksi kirjaimiksi. Tämä saattaa olla hyödyllistä esimerkiksi vertaillessa erilaisia merkkijonoja. Onneksi Swift tarjoaa helpon ja tehokkaan tavan muuttaa merkkijonon kaikki kirjaimet pieniksi.

## Miten tehdä se?

Voimme käyttää funktiota `lowercased ()`, joka on saatavana kaikissa String muuttujissa Swiftissä. Katso alla oleva koodiesimerkki ja sen tuottama tulos:

```Swift
var teksti = "MOI MAAILMA!"
print (teksti.lowercased ()) // tulostaa "moi maailma!"
```

Se on niin helppoa! Voimme myös tallentaa uuden String muuttujan arvon `lowercased ()` funktiolla:

```Swift
var teksti = "MOI MAAILMA!"
var alakirjaimet = teksti.lowercased ()
print (alkakirjaimet) // tulostaa "moi maailma!"
```

Voimme myös käyttää `lowercased ()` funktiota yhdessä ehtolauseen kanssa, kuten tässä esimerkissä:

```Swift
var oikeaSana = "kissa"
var käyttäjänSana = "KIssA"

if käyttäjänSana.lowercased () == oikeaSana { // vertaa alakirjaimisiin
    print ("Oikea vastaus!") // tulostaa "Oikea vastaus!"
} else {
    print ("Väärä vastaus!")
}
```

## Syvemmälle String alakirjaimisiin muuntamiseen

`lowercased ()` funktio käyttää taustalla Unicode:n yleisiä tallennussääntöjä muuttaessaan merkkijonon kirjaimet pieniksi. Se ottaa huomioon myös erikoismerkit ja erilaiset kieliympäristöt, joten se on luotettava tapa muuttaa merkkijonon kaikki kirjaimet pieniksi.

## Katso myös

- [Swift String dokumentaatio](https://developer.apple.com/documentation/swift/string)
- [String menetelmät ja Operatiiviset](https://www.noviello.it/string-methods-operators-swift-en/)