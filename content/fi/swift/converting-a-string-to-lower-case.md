---
title:                "Merkkijonon muuntaminen pienaakkosiksi"
html_title:           "Swift: Merkkijonon muuntaminen pienaakkosiksi"
simple_title:         "Merkkijonon muuntaminen pienaakkosiksi"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/swift/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Miksi

Miksi haluaisit muuttaa merkkijonon pieniksi kirjaimiksi? Yksi syy tähän voisi olla, että haluat vertailla merkkijonon eri muotoja, mutta haluat vertailun olevan case-insensitive.

## Miten

```Swift
let isoSana = "ELÄIN"
let pienetKirjaimet = isoSana.lowercased()
print(pienetKirjaimet)

// Output: eläin
```

Käytä `lowercased()`-metodia merkkijonon muuttamiseen pieniksi kirjaimiksi. Tämä metodi palauttaa uuden merkkijonon, joten voit tallentaa sen uuteen muuttujaan tai käyttää sitä suoraan tulostamiseen.

```Swift
let isoSana = "ELÄIMET OVAT IHANIA!"
let pienetKirjaimet = isoSana.lowercased()
print(pienetKirjaimet)

// Output: eläimet ovat ihania!
```

Huomaa, että `lowercased()`-metodi ei muuta alkuperäistä merkkijonoa, vaan palauttaa uuden muutetun merkkijonon.

## Syvällisempi sukellus

Merkkijonojen muuttaminen pieniksi kirjaimiksi on tärkeä osa jokapäiväistä ohjelmointia. Esimerkiksi käyttäjän antamat syötteet halutaan usein muuttaa pieniksi kirjaimiksi, jotta syötteen case ei vaikuta sovelluksen toimintoihin. `lowercased()`-metodi käyttää Unicode Standardin mukaista algoritmiä muuttaessaan merkkijonon kirjaimia pieniksi.

## Katso myös

- [Swiftin merkkijonojen käsittely](https://developer.apple.com/swift/blog/?id=2)
- [Apple Developer - String](https://developer.apple.com/documentation/swift/string)