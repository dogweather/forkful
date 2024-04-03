---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:08:45.200620-07:00
description: "Kuinka tehd\xE4\xE4n: Swiftin Foundation-runko tarjoaa `FileManager`-luokan,\
  \ jossa on menetelmi\xE4 tiedostoj\xE4rjestelm\xE4n hallintaan. Voit k\xE4ytt\xE4\
  \xE4\u2026"
lastmod: '2024-03-13T22:44:56.921024-06:00'
model: gpt-4-0125-preview
summary: "Swiftin Foundation-runko tarjoaa `FileManager`-luokan, jossa on menetelmi\xE4\
  \ tiedostoj\xE4rjestelm\xE4n hallintaan."
title: Tarkistetaan, onko hakemisto olemassa
weight: 20
---

## Kuinka tehdään:
Swiftin Foundation-runko tarjoaa `FileManager`-luokan, jossa on menetelmiä tiedostojärjestelmän hallintaan. Voit käyttää `FileManager`-luokkaa tarkistaaksesi, onko hakemisto olemassa. Tässä on pätkä siitä, miten tämä tehdään:

```swift
import Foundation

let fileManager = FileManager.default
let path = "/path/to/your/directory"

if fileManager.fileExists(atPath: path, isDirectory: nil) {
    print("Hakemisto on olemassa")
} else {
    print("Hakemistoa ei ole olemassa")
}
```

Tämä kuitenkin tarkistaa sekä tiedostot että hakemistot. Jos haluat erityisesti varmistaa, että hakemisto on olemassa, sinun tulee antaa osoitin Bool-arvoon `isDirectory`-parametriin:

```swift
import Foundation

let fileManager = FileManager.default
let path = "/path/to/your/directory"
var isDirectory: ObjCBool = false

if fileManager.fileExists(atPath: path, isDirectory: &isDirectory), isDirectory.boolValue {
    print("Hakemisto on olemassa")
} else {
    print("Hakemistoa ei ole olemassa")
}
```

### Kolmannen osapuolen kirjaston käyttö
Tällä hetkellä hakemiston olemassaolon tarkistaminen Swiftissä ei yleensä vaadi kolmannen osapuolen kirjastoja `FileManager`-luokan vankkuuden vuoksi. Kuitenkin monimutkaisempaan tiedostonhallintaan ja tarkistukseen, kirjastot, kuten John Sundellin **Files**, tarjoavat enemmän Swift-ystävällisen API:n.

Tässä on esimerkki sen käytöstä:

Ensiksi, lisää Files projektiisi Swift Package Managerin kautta.

Sitten, voit tarkistaa hakemiston olemassaolon näin:

```swift
import Files

do {
    _ = try Folder(path: "/path/to/your/directory")
    print("Hakemisto on olemassa")
} catch {
    print("Hakemistoa ei ole olemassa")
}
```

Huom: Koska kolmannen osapuolen kirjastot voivat muuttua, viittaa aina viimeisimpään dokumentaatioon käyttötavoista ja parhaista käytännöistä.
