---
title:                "Tiedoston kirjoittaminen"
html_title:           "Swift: Tiedoston kirjoittaminen"
simple_title:         "Tiedoston kirjoittaminen"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/swift/writing-a-text-file.md"
---

{{< edit_this_page >}}

# Mistä & Miksi?

Kirjoittaminen tekstitiedostoon on yksi tapa tallentaa tietoa tietokoneella. Ohjelmoijat käyttävät sitä esimerkiksi tallentaakseen käyttäjien syöttämiä tietoja tai luodakseen lokitiedostoja ohjelmiston toiminnasta.

# Miten toimia:

Luo uusi tiedosto nimettynä "tiedosto.txt" ja kirjoita siihen "Hei maailma!" seuraavasti:

```Swift
let teksti = "Hei maailma!"
do {
    try teksti.write(toFile: "tiedosto.txt", atomically: true, encoding: String.Encoding.utf8)
} catch {
    print("Tiedoston kirjoittaminen epäonnistui")
}
```
Tämän jälkeen voit tarkastella, että teksti on tallentunut tiedostoon avaamalla sen tekstieditorilla.

# Syväsukellus:

Kirjoittaminen tekstitiedostoon on ollut tapana jo pitkään tietokoneiden historiassa. Nykyään on olemassa myös muita tapoja tallentaa tietoa, kuten tietokannat ja pilvipalvelut. Swift-ohjelmointikielen lisäksi myös muut kielet, kuten Python ja C++, tarjoavat mahdollisuuden kirjoittaa tiedostoihin.

# Katso myös:

Lisätietoja tekstitiedostojen kirjoittamisesta löytyy Swiftin dokumentaatiosta osoitteesta https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html#ID339. Voit myös tutustua muihin tapoihin tallentaa tietoa, kuten tietokantoihin ja pilvipalveluihin.