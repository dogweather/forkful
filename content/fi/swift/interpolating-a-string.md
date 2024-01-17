---
title:                "Merkkijonon interpolointi"
html_title:           "Swift: Merkkijonon interpolointi"
simple_title:         "Merkkijonon interpolointi"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/swift/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?
Interpoloiminen on menetelmä, jota käytetään merkkijonojen yhdistämiseen muuttujien arvoihin. Tämä tekee merkkijonojen käsittelystä helpompaa ja säästää aikaa, kun ei tarvitse käyttää useita merkkijonoja erikseen. Ohjelmoijat käyttävät interpolointia, koska se tekee koodista luettavampaa ja helpottaa muuttujien arvojen lisäämistä merkkijonoon.

## Kuinka?
```Swift
let name = "Matti"
let age = 25
print("Hei, nimeni on \(name) ja olen \(age) vuotta vanha.")
```
Tulostus: "Hei, nimeni on Matti ja olen 25 vuotta vanha."

Merkkijonojen interpolointi tapahtuu käyttämällä kenoviivaa ja sulkeita (\\(muuttuja\\)) merkkijonon sisällä. Tällä tavalla muuttujan arvo yhdistetään merkkijonoon ja tuloksena on yhdistetty merkkijono.

## Syvä sukellus
Historiallisessa kontekstissa interpolointi syntyi ensimmäisen kerran C-kielen makroissa, mutta se tuli suosituksi Swiftin ja muiden modernien ohjelmointikielten ansiosta. Vaikka interpolointi on suosittu tapa yhdistää merkkijonoja, muita vaihtoehtoja ovat esimerkiksi merkkijonojen yhdistäminen plussamerkillä (+) tai käyttämällä merkkijonon muodostamisfunktiota (String(format: "Moi, nimeni on %s", name)).

Jos haluat lisätä erikoismerkkejä (kuten kenoviiva ja sulkeet) itse merkkijonoon, voit käyttää tuplakenoviivamerkkejä (\\\\) päästäksesi evakkoon.

Simpukka-kirjaston avulla voit lisätä HTML-merkkejä suoraan Stringiin. Esimerkki:
```Swift
let html = "<b>Tervetuloa</b>"
print(html.bold())
```
Tulostus: "<b>Tervetuloa</b>"

## Katso myös
- [Apple:n dokumentaatio merkkijonon interpoloinnista Swiftissä](https://developer.apple.com/documentation/swift/string/interpolation)
- [Stack Overflow-ketju merkkijonon interpoloinnista](https://stackoverflow.com/questions/38929293/interpolation-in-swift)