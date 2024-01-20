---
title:                "Tarkistetaan, onko hakemisto olemassa"
html_title:           "Lua: Tarkistetaan, onko hakemisto olemassa"
simple_title:         "Tarkistetaan, onko hakemisto olemassa"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/swift/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?

Tarkistamme onko hakemisto olemassa, jotta tiedämme voimmeko tallentaa tiedostoja sinne vai ei. Tämä on tarpeellista, koska ohjelman pitää välttää virheitä, joita syntyy, jos se yrittää kirjoittaa hakemistoon, jota ei ole olemassa.

## Miten Tehdään:

Swiftin käyttö huom! Tässä esimerkissä käytetään FileManager-olio:

```Swift
import Foundation

let fileManager = FileManager.default
let dirPath = "/Users/testDir/"

if fileManager.fileExists(atPath: dirPath) {
    print("Hakemisto on olemassa")
} else {
    print("Hakemisto ei ole olemassa")
}
```

Jos hakemisto on olemassa, tulostetaan "Hakemisto on olemassa". Muussa tapauksessa tulostetaan "Hakemisto ei ole olemassa"

## Syvempi Sukellus:

Historia: Swiftissa, FileManager käyttää peräisin olevaa C-kirjastoa tarkistaakseen, onko tiedosto tai hakemisto olemassa.

Vaihtoehdot: Voit myös käyttää `attributesOfItem(atPath:)` -menetelmää saadaksesi tarkempia tietoja hakemistosta.

Toteutus: Swiftin FileManager käyttää `fileExists(atPath:)` -metodia, joka on hyvin suoraviivainen ja helposti ymmärrettävä. Koodi yksinkertaisesti tarkistaa, onko hakemistopolku olemassa tiedostojärjestelmässä.

## Katso Myös:

1. Swiftin virallinen dokumentaatio, jossa on lisätietoja FileManager-luokasta ja sen metodeista: [https://developer.apple.com/documentation/foundation/filemanager](https://developer.apple.com/documentation/foundation/filemanager)
2. StackOverflow keskustelu, jossa käsitellään tarkistamista, onko hakemisto tai tiedosto olemassa Swiftissä: [https://stackoverflow.com/questions/24097826/read-and-write-data-from-text-file](https://stackoverflow.com/questions/24097826/read-and-write-data-from-text-file)