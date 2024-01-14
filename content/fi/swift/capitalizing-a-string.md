---
title:                "Swift: Merkkijonon muuttaminen alkavaksi isolla alkukirjaimella"
programming_language: "Swift"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/swift/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Miksi

Miksi haluaisit muuttaa merkkijonon ensimmäisen kirjaimen isoksi?

## Kuinka tehdä se

```Swift
let teksti = "tämä on esimerkkiteksti"
print(teksti.capitalized)
```

Tämä tulostaisi "Tämä On Esimerkkiteksti".

## Syvemmälle aiheeseen

Merkkijonon ensimmäisen kirjaimen muuttaminen isoksi on hyödyllinen toiminto monissa tilanteissa, kuten kun haluat muokata käyttäjän syöttämää tekstiä ennen sen tallentamista tietokantaan. Tämä toiminto helpottaa myös tekstin lukemista ja ymmärtämistä.

Merkkijonoilla on Swiftissä useita erilaisia toimintoja, kuten `capitalized`, `lowercased` ja `uppercased`. `Capitalized` toiminto muuttaa merkkijonon ensimmäisen kirjaimen isoksi ja `lowercased` muuttaa kaikki kirjaimet pieniksi. `Uppercased` taas muuttaa kaikki kirjaimet suuriksi.

On myös mahdollista muuttaa vain merkkijonon ensimmäinen kirjain isoksi käyttämällä `prefix`-toimintoa.

## Katso myös

Tässä muutama hyödyllinen linkki, jotka voivat auttaa sinua syventymään lisää merkkijonotoimintoihin Swiftissä:

- [Apple Developer Documentation - String](https://developer.apple.com/documentation/swift/string)
- [Hacking with Swift - How to capitalize words in a string using capitalized()](https://www.hackingwithswift.com/example-code/strings/how-to-capitalize-words-in-a-string-using-capitalized)