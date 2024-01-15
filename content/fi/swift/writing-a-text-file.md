---
title:                "Kirjoittaminen tekstitiedostoon"
html_title:           "Swift: Kirjoittaminen tekstitiedostoon"
simple_title:         "Kirjoittaminen tekstitiedostoon"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/swift/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Miksi

Tekstitiedostojen kirjoittaminen on hyvä taito jokaiselle Swift-ohjelmoijalle, sillä se on olennainen osa tiedon tallentamista ja käsittelyä. Lisäksi kirjoittaminen tekstitiedostoon on nopeampaa ja helpompaa kuin käyttäjän syötteen pyytäminen jokaisella suorituksella.

## Miten

Markdown language voidaan käyttää helposti tekstin kirjoittamiseen Swiftissä. Se mahdollistaa tekstin muotoilun ja järjestämisen helposti ymmärrettävässä muodossa.
```
Swift
let text = "Tervetuloa Swift-ohjelmoijaksi!"
do {
  try text.write(to: fileURL, atomically: true, encoding: .utf8)
  print("Teksti on tallennettu tiedostoon.")
} catch {
  print("Virhe tallentaessa tekstiä tiedostoon: \(error)")
}
```

## Syvempi sukellus

Tekstitiedoston kirjoittaminen Swiftissä tapahtuu käyttäen "write(to:atomically:encoding:)" -metodia, joka vaatii vähintään kolme parametria: tiedoston polku, atomisuus ja koodaus. Atomisuus viittaa siihen, tallennetaanko tiedosto kokonaisuudessaan vai ainoastaan osittain ja koodaus määrittää tiedoston merkkikoodauksen.

On myös hyvä muistaa, että tekstitiedoston kirjoittaminen ei ole mahdollista kaikissa tapauksissa, esimerkiksi silloin kun käytetään sandbox-ympäristöä. Tällöin voi olla tarpeen käyttää esimerkiksi FileManageriä tai Core Dataa tiedon tallentamiseen.

## Katso myös

* [Swiftin dokumentaatio tekstitiedoston kirjoittamisesta](https://developer.apple.com/documentation/foundation/nsstring/1412093-write)
* [Markdownin perusteet](https://www.markdownguide.org/basic-syntax/)