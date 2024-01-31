---
title:                "Merkkijonon muuttaminen isoiksi kirjaimiksi"
date:                  2024-01-19
html_title:           "Arduino: Merkkijonon muuttaminen isoiksi kirjaimiksi"
simple_title:         "Merkkijonon muuttaminen isoiksi kirjaimiksi"

category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/swift/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?
Tekstijonojen suuraakkostaminen tarkoittaa joko koko merkkijonon tai vain sanojen ensimmäisten kirjainten muuttamista suuriksi kirjaimiksi. Ohjelmoijat suuraakkostavat tekstejä käyttäjäliittymissä parantaakseen luettavuutta tai täyttääkseen teknisiä vaatimuksia.

## Miten:
```swift
let pieniTeksti = "hei maailma"
let suurakirjaimilla = pieniTeksti.uppercased() // Muuttaa kaikki kirjaimet suuriksi
print(suurakirjaimilla) // "HEI MAAILMA"

let isoAlkukirjain = pieniTeksti.capitalized // Suuri alkukirjain jokaiselle sanalle
print(isoAlkukirjain) // "Hei Maailma"
```

## Syväsukellus:
Tekstien suuraakkostaminen on ollut käytössä jo varhaisista käyttöjärjestelmistä ja ohjelmointikielistä lähtien, sillä se on yksi perustoiminnoista merkkijonojen käsittelyssä. Swiftissä `uppercased()` ja `capitalized` ovat `String`-tyypin metodeja, jotka hoitavat tämän. Vaihtoehtoisesti voit käyttää matalan tason funktioita muokkaamaan merkkijonoja mielivaltaisemmilla tavoilla. Tietääksesi, mitä taustalla tapahtuu: `uppercased()` käy läpi jokaisen merkin ja muuttaa ne vastaaviksi suuriksi kirjaimiksi Unicode-standardin mukaisesti, kun taas `capitalized` tekee tämän vain sanojen ensimmäisille kirjaimille.

## Katso myös:
- Swiftin virallinen dokumentaatio `String`-tyypistä: [https://developer.apple.com/documentation/swift/string](https://developer.apple.com/documentation/swift/string)
- Unicode-standardi ja merkkien käsittely: [https://unicode.org](https://unicode.org)
- Swiftin ohjelmoijan opas (String Handling): [https://swift.org/documentation/#the-swift-programming-language](https://swift.org/documentation/#the-swift-programming-language)
