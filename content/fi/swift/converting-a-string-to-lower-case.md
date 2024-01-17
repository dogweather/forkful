---
title:                "Merkkijonon muuttaminen pienaakkosiksi"
html_title:           "Swift: Merkkijonon muuttaminen pienaakkosiksi"
simple_title:         "Merkkijonon muuttaminen pienaakkosiksi"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/swift/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Mikä ja miksi?

Konversio merkkijonon pienaakkosiksi on prosessi, jossa muutetaan kaikki merkit merkkijonossa pienaakkosiksi. Tätä tehdään yleensä siksi, että ohjelmassa on tarve vertailla merkkijonoja keskenään, eikä kirjainkokoerojen haluta vaikuttavan vertailun tulokseen.

## Miten:

```Swift
let sana = "Tervetuloa maailma"
print(sana.lowercased())
// tulostaa: tervetuloa maailma
```

## Syväsukellus:

Merkkijonojen konversio pienaakkosiksi on ollut tarpeellista jo alkuaikoina, kun käyttöjärjestelmissä ei ollut tukia erilaisille kirjainkokoille. Nykyäänkin se on hyödyllistä esimerkiksi ohjelman saamiseksi palvelimella suoritettavaksi, sillä useimmat palvelimet käyttävät Linux-käyttöjärjestelmää, joka ei tue automaattisesti erilaisia kirjainkokoja. Toisena vaihtoehtona merkkijonon pienaakkosiksi muuttamiselle on käyttää `uppercased()`-metodia, joka muuttaa kaikki merkit isoiksi kirjaimiksi.

## Katso myös:

Voit lukea lisää merkkijonojen käsittelystä Swiftissä [Swift official docs](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html) ja [Hacking with Swift](https://www.hackingwithswift.com/quick-start/understanding-swift/why-does-swift-have-so-many-string-types).