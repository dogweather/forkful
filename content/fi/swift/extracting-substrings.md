---
title:                "Alimerkkijonojen poiminta"
html_title:           "Gleam: Alimerkkijonojen poiminta"
simple_title:         "Alimerkkijonojen poiminta"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/swift/extracting-substrings.md"
---

{{< edit_this_page >}}

# Alakielet Swift-ohjelmassa: Mikä se on ja miksi se on tärkeää

## Mitä & Miksi?

Alakielten (substring) erottaminen tarkoittaa suuremmasta merkkijonosta tietyn osan poimimista. Se on avain rooli ohjelmoinnissa, sillä sen avulla voidaan käsitellä ja manipuloida tekstiä tarkasti ja tehokkaasti.

## Toteutus:

Swift ohjelmistossa alakielien erottaminen voidaan suorittaa nopeasti ja helposti. Tässä on yksinkertainen esimerkki:

```Swift
let tervehdys = "Hei, maailma!"
let tervehdysAlaosa = String(tervehdys[tervehdys.index(tervehdys.startIndex, offsetBy: 4)..<tervehdys.endIndex])

print(tervehdysAlaosa)
```

Tämä tuottaa seuraavan tulosteen:

```Swift
", maailma!"
```

## Syvä sukellus:

Historiallisessa kontekstissa substringeille on ollut rajallisesti tukea monissa ohjelmointikielissä, mutta Swift on tehnyt sen erittäin helppokäyttöiseksi.

Vaihtoehtoja alaosioiden erottamiseen Swiftistä ovat esimerkiksi `prefix(_:)` ja `suffix(_:)` merkkijonomerkin funktiot, jotka ovat tehokkaampia tietyn pituisten alaosioiden poimimiseen.

Implementoinnin suhteen alakielen erottaminen Swiftissä on joustavaa. Käytämme `index(_:offsetBy:)` -metodia löytämään alueen, jonka haluamme poimia, ja asetamme sen uudeksi merkkijonoksi.

## Lisätietoja:

Lisää tietoa ja koodiesimerkkejä alakielten erottamisesta Swiftissä voit löytää alla olevista linkeistä:

- Swiftin virallinen dokumentointi: [Merkkijonot ja merkit](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
- Swift by Sundell: [Käyttämällä kielten ja osakielten](https://www.swiftbysundell.com/tips/using-string-and-substring-effectively/)