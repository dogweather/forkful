---
title:                "Swift: Merkkijonon muuntaminen pieniksi kirjaimiksi"
simple_title:         "Merkkijonon muuntaminen pieniksi kirjaimiksi"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/swift/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Miksi

Miksi muuttaa merkkijono pieniksi kirjaimiksi? Pieniksi kirjaimiksi muuttaminen on tärkeä osa merkkijonojen käsittelyä, ja se tekee tekstin vertailusta ja hakemisesta helpompaa. Esimerkiksi käyttäjien antamat merkkijonot voivat sisältää niin isoja kuin pieniäkin kirjaimia, joten muuttamalla ne kaikki pieniksi kirjaimiksi voimme varmistaa yhdenmukaisuuden ja helpottaa koodin lukemista.

## Kuinka tehdä

Pieniksi kirjaimiksi muuttaminen Swiftissä on hyvin yksinkertaista. Käytämme siihen String-tyypin lowercased() -metodia. Tässä esimerkissä muutamme merkkijonon "Tämä On Esimerkki" pieniksi kirjaimiksi ja tulostamme sen konsoliin.

```Swift
let merkkijono = "Tämä On Esimerkki"
print(merkkijono.lowercased())

/* Output:
tämä on esimerkki
*/
```

Voimme myös tallentaa muuttuneen merkkijonon uuteen muuttujaan, jos haluamme käyttää sitä myöhemmin koodissa.

```Swift
let merkkijono = "Tämä On Esimerkki"
let pienetKirjaimet = merkkijono.lowercased()
print(pienetKirjaimet)

/* Output:
tämä on esimerkki
*/
```

## Syvempi sukellus

Mikäli haluamme muuntaa merkkijonon vain tiettyyn kieleen sopivaksi, voimme käyttää lowercased() -metodin sijasta localizedLowercase -metodia. Tämä huomioi kyseisen kielen merkistön ja muuntaa merkkijonon vastaaviksi pieniksi kirjaimiksi.

```Swift
let merkkijono = "ÅÄÖ"
print(merkkijono.localizedLowercase)

/* Output:
åäö
*/
```

Lisäksi Swift tarjoaa myös uppercased() ja uppercased() -metodit, jotka muuntavat merkkijonon isoiksi kirjaimiksi.

## Katso myös

- [Swiftin virallinen String-dokumentaatio](https://developer.apple.com/documentation/swift/string)
- [Apple Education: Exploring Strings, Characters, and Collections](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)