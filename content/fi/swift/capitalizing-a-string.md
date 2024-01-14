---
title:                "Swift: Merkkijonon kirjoitusasu"
simple_title:         "Merkkijonon kirjoitusasu"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/swift/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Miksi

Kirjoittaessa Swift-koodia, saattaa joskus olla tarpeen muuttaa merkkijono isolla alkukirjaimella. Tämä voi olla esimerkiksi silloin kun haluamme tulostaa tekstin otsikkona tai esiintuoda tietokannassa olevia tietoja.

## Miten

Voit muuttaa merkkijonon ensimmäisen kirjaimen isolla alkukirjaimella käyttämällä `capitalized`-funktiota. Tämä funktio ottaa merkkijonon ja palauttaa uuden merkkijonon, jossa ensimmäinen kirjain on iso.

```Swift
let nimi = "johanna"
print(nimi.capitalized) // Tulostaa Johanna
```

Jos haluat muuttaa koko merkkijonon isoksi, voit käyttää `uppercased`-funktiota.

```Swift
let tervetuloa = "Tervetuloa"
print(tervetuloa.uppercased) // Tulostaa TERVETULOA
```

On myös mahdollista muuttaa vain ensimmäinen kirjain isoksi ja loput kirjaimet pieniksi käyttämällä `capitalizingFirstLetter`-funktiota.

```Swift
let eläin = "kARVAS"
print(eläin.capitalizingFirstLetter) // Tulostaa Karvas
```

## Syvällisempi tarkastelu

Merkkijonojen käsittely Swiftissä onnistuu kätevästi erilaisten funktioiden avulla. `capitalized`-funktio perustuu Unicode-standardiin, joten se toimii oikein myös erikoismerkkien ja muiden kielten kanssa. 

Lisäksi merkkijonoja voi muokata myös monilla muilla tavoilla. Esimerkiksi `trimmingCharacters`-funktio poistaa merkkijonon alku- ja loppupuolelta halutut merkit ja `replacingOccurrences`-funktio korvaa halutut merkit toisilla. 

## Katso myös

- [Swiftin virallinen dokumentaatio merkkijonojen käsittelystä](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
- [Muita hyödyllisiä merkkijonojen käsittelyyn liittyviä funktioita](https://www.hackingwithswift.com/articles/105/how-to-use-string-interpolation-in-swift)
- [Kätevä opas merkkijonojen käsittelystä Swiftissä](https://www.appcoda.com/swift-string/)