---
title:                "Swift: Säännöllisten ilmaisujen käyttö"
simple_title:         "Säännöllisten ilmaisujen käyttö"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/swift/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Miksi käyttää säännöllisiä lausekkeita Swift-ohjelmoinnissa?

Säännölliset lausekkeet ovat voimakas työkalu, joka helpottaa datan käsittelyä ja manipulointia Swift-ohjelmoinnissa. Ne ovat erityisen hyödyllisiä, kun halutaan suorittaa monimutkaisia hakutehtäviä, kuten tietyn kaavan mukaisten merkkijonojen etsimistä.

## Kuinka käyttää säännöllisiä lausekkeita Swift-ohjelmoinnissa?

Seuraavassa esimerkissä näytämme, kuinka käyttää säännöllisiä lausekkeita etsimään kaikki puhelinnumerot annetusta tekstistä:

```Swift
let text = "Ota yhteyttä minuun numeroon 040-123456 tai sähköpostitse osoitteeseen example@test.com"

let pattern = "[0-9]{3}-[0-9]{6}"

if let range = text.range(of: pattern, options: .regularExpression) {
    let phoneNumber = text[range]
    print("Löydettiin puhelinnumero: \(phoneNumber)")
}

// Output: Löydettiin puhelinnumero: 040-123456
```

Koodissa luomme ensin muuttujan *text*, jossa on haluamme etsiä puhelinnumeroita. Sitten luomme muuttujan *pattern*, jossa määritämme säännöllisen lausekkeen, joka vastaa puhelinnumeroa (kolme numeroa viiva kuusi numeroa). Lopuksi käytämme *range* -metodia etsimään tekstistä *text* *pattern* -muuttujasta ja tulostamme löydetyn puhelinnumeron.

## Syvemmälle säännöllisten lausekkeiden käyttöön Swift-ohjelmoinnissa

Säännöllisten lausekkeiden käyttö Swift-ohjelmoinnissa voi olla monimutkaista, mutta niiden avulla voi suorittaa monia tehtäviä, kuten tietyn kaavan mukaisten merkkijonojen etsimistä, tiedostonimien tarkistamista, puhelinnumeroiden tunnistamista ja paljon muuta. On tärkeää ymmärtää erilaisia säännöllisten lausekkeiden merkintätapoja ja käyttää niitä oikein halutun tuloksen saavuttamiseksi.

## Katso myös

- [Swift-säännölliset lausekkeet -dokumentaatio](https://developer.apple.com/documentation/swift/regular_expression)
- [Regex-tutoriaali Swift-ohjelmoijille](https://www.swiftdiv.com/swift-regular-expression-tutorial/)
- [Säännöllisten lausekkeiden testaustyökalu](https://regexr.com/)