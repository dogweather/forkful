---
title:                "Swift: Kansiovapauden tarkistaminen"
programming_language: "Swift"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/swift/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Miksi tarkistaa hakemiston olemassaolo?

Hakemistojen tarkistaminen on tärkeä osa ohjelmointia, sillä se mahdollistaa tiedostojärjestelmän hallinnan ja tiedostojen käsittelyn. Tarkistamalla hakemiston olemassaolon varmistetaan, että ohjelma toimii suunnitellusti ja että kaikki tarvittavat tiedostot ovat saatavilla.

## Miten tarkistaa hakemiston olemassaolo?

```
Swift func checkDirectoryExists(atPath path: String) -> Bool {
    let fileManager = FileManager.default
    var isDirectory: ObjCBool = true
    return fileManager.fileExists(atPath: path, isDirectory: &isDirectory)
}
```
Tämä koodiesimerkki näyttää yksinkertaisen funktion tarkistamaan, onko hakemisto olemassa annetussa polussa. Funktion avulla voidaan helposti tarkistaa, onko hakemisto olemassa ja saada tieto siitä, onko kyseessä hakemisto vai tiedosto.

Käyttöesimerkki:
```
Swift let directoryExists = checkDirectoryExists(atPath: "/Users/Name/Documents")
if directoryExists {
    print("Hakemisto löytyy.")
} else {
    print("Hakemisto ei ole olemassa.")
}
```
Tulostus:
```
Hakemisto löytyy.
```

## Syvempi sukellus

Hakemistojen tarkistaminen voi olla hyödyllistä myös muiden tehtävien yhteydessä, kuten tiedostojen luomisessa tai poistamisessa. On myös hyvä muistaa, että joskus hakemisto saattaa olla olemassa, mutta siinä ei ole tarvittavia oikeuksia tiedostojen käsittelemiseen. Tällöin tarkistaminen auttaa välttämään mahdollisia virheitä.

## Katso myös

- [Apple Developer Documentation: FileManager](https://developer.apple.com/documentation/foundation/filemanager)
- [Swift by Sundell: Working with files and directories in Swift](https://www.swiftbysundell.com/basics/working-with-files-and-directories/)