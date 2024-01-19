---
title:                "Tilapäisen tiedoston luominen"
html_title:           "Arduino: Tilapäisen tiedoston luominen"
simple_title:         "Tilapäisen tiedoston luominen"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/swift/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?

Tilapäisten tiedostojen luominen tarkoittaa sellaisten tiedostojen luomista, joita ohjelma tarvitsee väliaikaisesti, mutta ei lopullisesti. Se on hyödyllistä esimerkiksi välimuistiin tallentamisessa tai suurien tiedostojen käsittelyn välietappeina. 

## Näin tehdään:

Swiftissa voimme luoda tilapäisen tiedoston seuraavasti:

```Swift
import Foundation

let tempDirectoryURL = URL(fileURLWithPath: NSTemporaryDirectory())
let tempFileURL = tempDirectoryURL.appendingPathComponent(UUID().uuidString)

do {
   try "Temporary File Content".write(to: tempFileURL, atomically: true, encoding: String.Encoding.utf8)
} catch {
   print("Failed to write to \(tempFileURL)")
}
```
Kun suoritat tämän koodin, se luo tilapäisen tiedoston laitteen väliaikaisten tiedostojen polkuun. Tiedoston nimenä toimii tuotettu satunnainen UUID ja sisältönä on "Temporary File Content".

## Syvempi sukellus:

Tilaisten tiedostojen käsitteen historia alkaa aikaan jolloin levytila oli kallista. Sen sijaan, että säilyttäisit tiedot loputtomasti, tiedot tallennettiin tilapäiseen tiedostoon ja vapautettiin levytila lopussa. Nykyaikana, vaikka levytilan saatavuus on parantunut, tilapäisten tiedostojen käyttö on yhä hyödyllistä laskentatehoja vaativissa tapauksissa tai tietoturvaan liittyvissä tapauksissa.

Vaihtoehtoisena tapana tiedostovirta- tai Piped-luokan käyttö voi tarjota samankaltaisen toiminnallisuuden ilman fyysisen tiedoston luontia. Tämä on hyödyllistä tapauksissa, joissa jatkuva tiedonsiirto on tarpeen.

Jokaisen tiedoston istunnon aikana generoidaan satunnainen UUID sen varmistamiseksi, että tiedoston nimi on yksilöllinen. Tämä myös auttaa välttämään mahdollisen tiedoston korvaamisen.

## Katso myös:

1. Apple Developer Documentation: [Foundation Framework Reference](https://developer.apple.com/documentation/foundation)
2. Stack Overflow: [Difference between tmp and var/tmp?](https://stackoverflow.com/questions/4550296/difference-between-tmp-and-var-tmp)
3. SwiftString API dokumentaatio: [UUID](https://developer.apple.com/documentation/foundation/uuid)