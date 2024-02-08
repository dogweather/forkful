---
title:                "Merkkijonon muuntaminen pieniksi kirjaimiksi"
aliases:
- fi/swift/converting-a-string-to-lower-case.md
date:                  2024-01-20T17:39:08.554349-07:00
model:                 gpt-4-1106-preview
simple_title:         "Merkkijonon muuntaminen pieniksi kirjaimiksi"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/swift/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## What & Why?
Tekstin muuttaminen pieniksi kirjaimiksi tarkoittaa kaikkien merkkijonon kirjainten muuntamista vastaaviksi pienaakkosiksi. Tätä tehdään yhtenäistämään dataa, esimerkiksi käyttäjän syötteen vertailussa tai hakutoiminnallisuuksissa.

## How to:
Swiftissä merkkijonon muuttaminen pieniksi kirjaimiksi on yksinkertaista. Käytä `lowercased()` metodia:

```Swift
let originalString = "Hei Maailma!"
let lowercasedString = originalString.lowercased()
print(lowercasedString)
```

Tulostuu: `hei maailma!`

## Deep Dive
Alkuperäisen Swift 1:ssä (2014), `.lowercaseString` oli tapa muuttaa merkkijonot pieniksi kirjaimiksi. Swift 3:ssa (2016) se muutettiin `.lowercased()` metodiksi, joka on ollut käytössä siitä lähtien. Alternatiiveja suoraan `.lowercased()` metodille ei ole, mutta voi olla tilanteita, joissa halutaan esimerkiksi säilyttää tietyt merkit sellaisinaan tai soveltaa kulttuurisensitiivistä pienten kirjaimien käyttöä, missä tarvitaan lisälogiikkaa. Swift tekee pienentämisen Unicode-standardin mukaisesti, joten se käsittelee laajan valikoiman kirjaimia eri kielistä.

## See Also
- Swiftin virallinen dokumentaatio merkkijonon käsittelyyn: [Swift String and Characters](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
- Unicode-standardi: [Unicode Character Database](https://www.unicode.org/ucd/)
