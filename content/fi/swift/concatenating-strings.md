---
title:                "Merkkijonojen yhdistäminen"
html_title:           "Gleam: Merkkijonojen yhdistäminen"
simple_title:         "Merkkijonojen yhdistäminen"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/swift/concatenating-strings.md"
---

{{< edit_this_page >}}

# Merkkijonojen yhdistäminen Swift ohjelmoinnissa

## Mikä ja Miksi?
Merkkijonojen yhdistäminen on prosessi, jossa liitetään kaksi tai useampia merkkijonoja yhdeksi. Ohjelmoijat tekevät tämän usein tiedon esittämiseksi ymmärrettävässä ja järjestetyssä muodossa.

## Kuinka toteuttaa:
Swiftin avulla voit yhdistää merkkijonoja yksinkertaisesti käyttämällä "+" -operaattoria. Katsotaan esimerkkiä:
```Swift
let string1 = "Hei"
let string2 = "Maailma"
let tervehdys = string1 + ", " + string2
print(tervehdys)  // Tulostus: "Hei, Maailma"
```
Toinen tapa yhdistää merkkijonoja on käyttämällä `append(_:)` -funktiota:
```Swift
var moikka = "Terve"
moikka.append("hdys!")
print(moikka)  // Tulostus: "Tervehdys!"
```

## Syvempi sukellus:
Historiallisesti Swiftin aikaisemmissa versioissa "+" -operaattoria käytettiin yleisimmin merkkijonojen yhdistämiseen. Nykyään Swiftin tuoreimmissa versioissa `append(_:)` -funktiota pidetään optimaalisempana, koska se ei luo uutta merkkijonoa, kuten "+" -operaattori tekee.

Vaihtoehtona voit käyttää `join(_:) ` -funktiota, jossa useita merkkijonoja yhdistetään tietyn erotinmerkkijonon avulla:
```Swift
let tervehdysLista = ["Hei", "Maailma"]
let tervehdys = tervehdysLista.joined(separator: ", ")
print(tervehdys)  // Tulostus: "Hei, Maailma"
```

## Katso myös:
1. Swiftin virallinen dokumentaatio merkkijonojen käsittelystä: [Swift.org](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html) 
2. StackOverflow-keskustelu merkkijonojen yhdistämisestä Swiftissä: [StackOverflow](https://stackoverflow.com/questions/24092884/how-to-use-string-concatenation-in-swift)