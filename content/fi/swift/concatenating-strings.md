---
title:                "Elatusjonojen yhdistäminen"
html_title:           "Swift: Elatusjonojen yhdistäminen"
simple_title:         "Elatusjonojen yhdistäminen"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/swift/concatenating-strings.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?

Stringien yhdistäminen on yksinkertainen mutta tärkeä osa Swift-ohjelmointia. Se tarkoittaa kahden tai useamman tekstin yhdistämistä yhdeksi merkkijonoksi. Tämä tehdään yleensä silloin, kun halutaan muodostaa pidempiä lauseita tai näyttää tietoja ohjelman käyttäjälle.

## Miten:

```
let firstName = "Matti"
let lastName = "Meikäläinen"

let fullName = firstName + " " + lastName

print(fullName)

```

```
//Tuloste: Matti Meikäläinen
```

## Syvempää tietoa:

Stringien yhdistäminen on ollut osa ohjelmointikieliä jo pitkään. Aikaisemmin sitä tehtiin usein eri tavalla, esimerkiksi käyttämällä erilaisia merkintätapoja. Swiftissä stringien yhdistäminen on yksinkertaista, kun käytetään plus-merkkiä (`+`) tai yhtäsuuruusmerkkiä (`+=`).

Mikäli halutaan liittää muuttujien arvot suoraan tekstiin, voidaan käyttää myös merkkijonon muotoilua, joka tunnetaan myös nimellä string-interpolation.

## Lisälukemista:

Voit lukea lisää stringien yhdistämisestä Swiftissä [Swiftin dokumentaatiosta](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html) tai [Hacking with Swift](https://www.hackingwithswift.com/) -sivustolta. Sieltä löytyy myös muita hyödyllisiä ohjeita Swift-ohjelmointiin.