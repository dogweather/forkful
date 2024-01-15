---
title:                "Merkkijonon muuttaminen isoiksi kirjaimiksi"
html_title:           "Swift: Merkkijonon muuttaminen isoiksi kirjaimiksi"
simple_title:         "Merkkijonon muuttaminen isoiksi kirjaimiksi"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/swift/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Miksi
Miksi joku haluaisi käyttää Swift-ohjelmointikieltä ja kirjoittaa koodia, joka muuttaa merkkijonon kirjaimet isoihin kirjaimiin?

## Miten tehdä se
Koodinpätkät ja esimerkkilähdöt "```Swift ... ```" koodilohkoissa.

Esimerkki 1:
```Swift
let sana = "hei"
print("Merkkijono isoin kirjaimin: \(sana.uppercased())")
```
**Tulostaa:** "Merkkijono isoin kirjaimin: HEI"

Esimerkki 2:
```Swift
let lause = "tämä on lause"
print("Lause isoin kirjaimin: \(lause.capitalized)")
```
**Tulostaa:** "Lause isoin kirjaimin: Tämä on lause"

## Syväluotaus
Merkkijonon kirjainten muuttaminen isoihin kirjaimiin voi olla hyödyllistä esimerkiksi silloin, kun käyttäjä syöttää tiedot pienillä kirjaimilla mutta ne halutaan tallentaa tietokantaan isoin kirjaimin. Swift-ohjelmointikielen sisäänrakennettu ominaisuus "uppercased()" muuttaa merkkijonon kaikki kirjaimet isoihin kirjaimiin, kun taas "capitalized" muuttaa vain ensimmäisen kirjaimen isoksi ja muut pieniksi. On myös mahdollista käyttää "lowercased()" muuttaaksesi kirjaimet pieniksi.

## Katso myös
- [Swiftin virallinen verkkosivusto](https://swift.org/)
- [Apple:n Swift-opetusohjelmat](https://developer.apple.com/swift/resources/)
- [Swift-koodin kirjoitusohjeet](https://swift.org/documentation/api-design-guidelines/)