---
title:                "Tekstitiedoston lukeminen."
html_title:           "Swift: Tekstitiedoston lukeminen."
simple_title:         "Tekstitiedoston lukeminen."
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/swift/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Mikä & miksi?
Tekstin tiedoston lukeminen on toiminto, jossa ohjelmoija lataa tiedoston sisällön lukuun ja käyttöön. Tätä tehdään yleensä siksi, että ohjelma voi käsitellä ja näyttää tiedoston sisällön käyttäjälle.

## Kuinka?
Swift-koodilla on helppo ladata tekstitiedosto ja näyttää sen sisältö. Katso alla olevia esimerkkejä ja niiden tulosteita.

```Swift
// Esimerkki 1: Tiedoston lukeminen ja tulostaminen
if let text = try? String(contentsOfFile: "tekstitiedosto.txt") {
    print(text)
}
```
```Swift
// Esimerkki 2: Tiedoston luku ja käsittely riveittäin
if let text = try? String(contentsOfFile: "tekstitiedosto.txt") {
    let lines = text.components(separatedBy: "\n")
    for line in lines {
        print(line)
    }
}
```
Tuloste esimerkissä 1:
```
Tämä on esimerkki tekstitiedoston sisällöstä.
Käytämme tätä tiedostoa näyttämään kuinka helppoa on lukea tekstitiedosto Swift-koodilla.
```
Tuloste esimerkissä 2:
```
Tämä on esimerkki tekstitiedoston sisällöstä.
Käytämme tätä tiedostoa näyttämään kuinka helppoa on lukea tekstitiedosto Swift-koodilla.
```

## Syventävä sukellus
Tekstitiedoston lukeminen on yleinen toiminto, joka on ollut käytössä ohjelmoinnissa jo pitkään. Aikaisemmin sitä tehtiin usein matalalla tasolla käyttäen C-kielestä peräisin olevia funktioita, mutta nykyään monet ohjelmointikielet, kuten Swift, tarjoavat helppokäyttöisiä työkaluja tekstitiedostojen käsittelyyn.

Jos tekstitiedoston lukeminen ei sovellu sinun tarpeisiisi, voit harkita muiden tiedostomuotojen käyttöä, kuten CSV- tai JSON-tiedostoja.

## Katso myös
- [Swiftin virallinen dokumentaatio tekstitiedoston käsittelyyn](https://developer.apple.com/documentation/foundation/filemanager/1407698-contents)
- [Ohjelmointiopas tekstitiedoston lukemiseen ja kirjoittamiseen Swiftillä](https://www.hackingwithswift.com/example-code/strings/how-to-read-a-whole-file-into-a-string-with-contentsof)
- [CS231n-opetusohjelma tekstitiedoston käsittelystä Pythonilla](https://cs231n.github.io/python-numpy-tutorial/#python-strings)