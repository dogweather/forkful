---
title:    "Swift: Tiedoston kirjoittaminen"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/swift/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Miksi Kirjoittaa Tekstitiedosto

Monet ohjelmoijat löytävät itsensä kirjoittamassa tekstitiedostoja, jotka sisältävät koodia, muistiinpanoja tai muita tärkeitä tietoja. Tekstitiedostot ovat hyödyllisiä, koska ne ovat yksinkertaisia, helppoja luoda ja helppoja jakaa muiden kanssa. Lisäksi ne ovat hyödyllisiä, kun haluat tallentaa tietoja, jotka eivät vaadi monimutkaisia ​​tietorakenteita.

## Kuinka kirjoittaa tekstitiedosto

Kirjoittaaksesi tekstitiedoston Swiftillä, sinun on ensin luotava uusi tiedosto ja annettava sille tiedostopääte ".txt". Tämän jälkeen voit käyttää ```Swift NSString ``` -luokkaa luomaan merkkijono, joka sisältää haluamasi tiedot. Sitten voit käyttää ```Swift FileManager ``` -luokkaa tallentamaan tiedoston haluamaasi sijaintiin.

```
let text = "Tämä on esimerkki tekstitiedoston kirjoittamisesta Swiftillä."
let fileManager = FileManager.default
let filePath = fileManager.urls(for: .documentDirectory, in: .userDomainMask)[0].appendingPathComponent("example.txt")
do {
   try text.write(to: filePath, atomically: false, encoding: .utf8)
} catch {
   print("Virhe tallennettaessa tiedostoa.")
}
```
Kaikki mitä tarvitsit tehdäksesi, oli luoda merkkijono ja tallentaa se haluamaasi tiedostoon. Voit muuttaa merkkijonoa, lisätä uusia rivejä ja tallentaa muutoksia uudelleen.

## Syvemmälle tekstitiedoston kirjoittamiseen

Tekstitiedostojen kirjoittaminen Swiftissä on yksinkertaista, mutta sinulla on myös mahdollisuus lisätä tiedostonhallintaa ja tarkistaa, onko tiedosto jo olemassa ennen tallentamista. Voit myös käyttää muita tietokäsittelijäluokkia, kuten ```Swift Data ``` -luokkaa, jos haluat tallentaa binääridataa.

On myös tärkeää muistaa, että tekstitiedoston luominen ei rajoitu vain Swiftiin. Voit käyttää samaa menetelmää myös muihin ohjelmointikieliin, kuten Javaan tai Pythoniin.

## Katso myös

- [Swiftin virallinen dokumentaatio tekstitiedostojen kirjoittamisesta](https://docs.swift.org/swift-book/LanguageGuide/PhrasesAndStatements.html#ID541)
- [Ohjelmointiopas tekstitiedostojen käsittelyyn Swiftissä](https://www.raywenderlich.com/23892657-writing-files-using-the-filemanager-class-in-swift)
- [GitHub-projekti, joka esittelee tekstitiedoston kirjoittamisen Swiftillä](https://github.com/meici/codeLab/tree/master/FileWriting)