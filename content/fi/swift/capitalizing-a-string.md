---
title:                "Merkkijonon pääkirjaintaminen"
html_title:           "Swift: Merkkijonon pääkirjaintaminen"
simple_title:         "Merkkijonon pääkirjaintaminen"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/swift/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?
Merkkijonojen isoilla alkukirjaimilla kirjoittaminen tarkoittaa sitä, että muutat jokaisen sanan ensimmäisen kirjaimen isoksi. Se tekee tiedoista helpommin luettavia ja ymmärrettäviä.

## Näin toimit:
Swiftissä merkkijonon kirjaimien muuttaminen isoksi on suoraviivaista `capitalized`-ominaisuuden avulla:

```swift
let tervehdys = "hei maailma"
let isoAlku = tervehdys.capitalized
print(isoAlku)
```

Ohjelmasta tuleva tulostus näyttää seuraavalta: 

```swift
"Hei Maailma"
```

## Syvempi Sukellus
Historiallisesti merkkijonon kirjaimien muuttaminen isoksi on tapahtunut erilaisilla tavoilla eri ohjelmointikielissä. Swiftin `capitalized`-ominaisuus on helppo ja nopea tapa saavuttaa tämä. Vaihtoehtoisesti, voit luoda oman funktion, joka käy läpi merkkijonon kirjaimet ja muuttaa ne isoksi.

Stringin "capitalized" ominaisuus Swiftissä itse asiassa käyttää NSLinguisticTagger-luokkaa tekemään kirjainkokoisen lokalisoinnin. Tämä tarkoittaa, että se ottaa huomioon kieliopilliset säännöt eri kielissä, mikä on hyödyllistä monikielisissä sovelluksissa.

## Katso myös
Edistä vastaavien taitojesi harjoittelua tutustumalla seuraaviin lähteisiin:

1. Swiftin dokumentaatio: [String](https://developer.apple.com/documentation/swift/string)
2. Kattava opas merkkijonojen käsittelyyn Swiftissä: [Hacking with Swift](https://www.hackingwithswift.com/read/0/6/strings) 
3. NSLinguisticTagger-luokka: [NSLinguisticTagger](https://developer.apple.com/documentation/foundation/nslinguistictagger)