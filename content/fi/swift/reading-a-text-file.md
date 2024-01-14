---
title:    "Swift: Tekstitiedoston lukeminen"
keywords: ["Swift"]
---

{{< edit_this_page >}}

## Miksi

Miksi haluaisit lukea teksti-tiedostoja ohjelmointikieltä käytettäessä? Teksti-tiedostojen lukeminen voi olla hyödyllistä, kun haluat käsitellä suuria määriä tietoa, jota ei ole tallennettu tietokantaan tai muuhun tietokoneen muistiin. Tämä voi olla erityisen hyödyllistä, kun haluat lukea käyttäjän antamia syötteitä tai käsitellä tietoja ulkoisesta lähteestä.

## Miten

```Swift
let path = "/Users/kayttaja/documents/tiedosto.txt"
do {
    let data = try String(contentsOfFile: path)
    print(data)
} catch {
    print("Tiedoston lukeminen epäonnistui.")
}
```

Yllä olevassa esimerkissä käytetään Swiftin `String`-luokan `contentsOfFile`-funktiota lukeaksesi teksti-tiedosto. `try` ja `catch`-lausekkeilla käsitellään virheellinen tilanne, jos tiedoston lukeminen epäonnistuu. Kun teksti-tiedosto on luettu, sen tiedot tallennetaan `data`-muuttujaan ja tulostetaan konsoliin.

## Syvempi sukellus

Tiedostojen lukeminen voi olla monimutkaisempaa kuin yksinkertaisesti koodin esittäminen. Voit esimerkiksi haluta lukea vain tietyt rivit tai sarakkeet tiedostosta tai käsitellä tiedoston sisältöä tietokannan tavoin. On myös tärkeää muistaa käsitellä tiedoston sulkeminen lauseen `contentsOfFile` käytön jälkeen, jotta vältetään vuotoja ja tietokoneen ylikuormitusta.

## Katso myös

- [Swiftin virallisen dokumentaation osoite](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
- [How to Read a Text File in Swift -ohjeet](https://iosdevcenters.blogspot.com/2015/12/how-to-read-text-file-in-swift.html)
- [NSScanner-luokan käyttö teksti-tiedostojen lukemiseen Swiftissä](https://stackoverflow.com/questions/2795096/split-string-into-subarrays-in-swift)