---
title:                "Tarkistetaan, onko hakemisto olemassa"
aliases: - /fi/swift/checking-if-a-directory-exists.md
date:                  2024-02-03T19:08:45.200620-07:00
model:                 gpt-4-0125-preview
simple_title:         "Tarkistetaan, onko hakemisto olemassa"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/swift/checking-if-a-directory-exists.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Mikä ja miksi?
Hakemiston olemassaolon tarkistaminen tiedostojärjestelmässä on oleellista, kun hallinnoit tiedostorakenteita Swift-sovelluksistasi käsin. Tämä tehtävä mahdollistaa kehittäjille hakemistojen läsnäolon varmistamisen ennen niistä lukemista tai niihin kirjoittamista, välttäen näin mahdolliset suoritusaikaiset virheet.

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
