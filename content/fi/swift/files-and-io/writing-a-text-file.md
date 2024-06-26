---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:30:36.460168-07:00
description: "Miten: Swiftin vakiokirjasto sis\xE4lt\xE4\xE4 kaikki tarvittavat ty\xF6\
  kalut tekstitiedostojen kirjoittamiseen. T\xE4ss\xE4 on perusl\xE4hestymistapa."
lastmod: '2024-03-13T22:44:56.924810-06:00'
model: gpt-4-0125-preview
summary: "Swiftin vakiokirjasto sis\xE4lt\xE4\xE4 kaikki tarvittavat ty\xF6kalut tekstitiedostojen\
  \ kirjoittamiseen."
title: Tekstitiedoston kirjoittaminen
weight: 24
---

## Miten:


### Käyttäen Swiftin vakiokirjastoa
Swiftin vakiokirjasto sisältää kaikki tarvittavat työkalut tekstitiedostojen kirjoittamiseen. Tässä on peruslähestymistapa:

```swift
import Foundation

let sisalto = "Hei, Wiredin lukijat! Swiftin opiskelu on hauskaa."
let tiedostoPolku = NSSearchPathForDirectoriesInDomains(.documentDirectory, .userDomainMask, true)[0] as String
let tiedostoNimi = "\(tiedostoPolku)/esimerkki.txt"

do {
    try sisalto.write(toFile: tiedostoNimi, atomically: false, encoding: String.Encoding.utf8)
    print("Tiedosto kirjoitettu onnistuneesti")
} catch let error as NSError {
    print("Kirjoittaminen URLiin epäonnistui: \(tiedostoNimi), Virhe: " + error.localizedDescription)
}
```

Tämä koodipätkä kirjoittaa merkkijonon nimellä `esimerkki.txt` sijaintiin dokumenttien kansio. Se käsittelee mahdolliset virheet käyttäen Swiftin do-try-catch-virheenkäsittelyä.

### Käyttäen FileManageria lisäkontrolliin
Lisäkontrolliin tiedoston attribuuteista tai tarkistaaksesi, olemassaoleeko tiedosto jo, voi käyttää `FileManageria`:

```swift
import Foundation

let tiedostonhallinta = FileManager.default
let hakemistot = tiedostonhallinta.urls(for: .documentDirectory, in: .userDomainMask)
if let dokumenttihakemisto = hakemistot.first {
    let tiedostoURL = dokumenttihakemisto.appendingPathComponent("esimerkki.txt")
    let sisalto = "Swiftin tutkiminen tiedostonhallintaa varten on valaisevaa."

    if tiedostonhallinta.fileExists(atPath: tiedostoURL.path) {
        print("Tiedosto on jo olemassa")
    } else {
        do {
            try sisalto.write(to: tiedostoURL, atomically: true, encoding: .utf8)
            print("Tiedosto luotu ja kirjoitettu onnistuneesti")
        } catch {
            print("Tiedoston kirjoitusvirhe: \(error)")
        }
    }
}
```

### Käyttäen kolmannen osapuolen kirjastoja
Yksi suosittu kolmannen osapuolen kirjasto tiedostojärjestelmätoimintojen käsittelyyn Swiftissä on `Files` John Sundelliltä:

Lisää ensin Files projektiisi, yleensä Swift Package Managerin kautta.

```swift
// swift-tools-version:5.3
import PackageDescription

let paketti = Package(
    name: "PaketinNimesi",
    dependencies: [
        .package(url: "https://github.com/JohnSundell/Files", from: "4.0.0"),
    ],
    targets: [
        .target(
            name: "KohdeNimesi",
            dependencies: ["Files"]),
    ]
)
```

Käytä sitten sitä kirjoittaaksesi tiedostoon:

```swift
import Files

do {
    let tiedosto = try File(path: "/polku/sinun/hakemistoosi/esimerkki.txt")
    try tiedosto.write(string: "Swift ja Files-kirjasto muodostavat tehokkaan yhdistelmän.")
    print("Tiedosto kirjoitettu onnistuneesti käyttäen Files-kirjastoa.")
} catch {
    print("Tapahtui virhe: \(error)")
}
```

`Files`-kirjaston avulla tiedostojen käsittely muuttuu suoraviivaisemmaksi, mikä mahdollistaa keskittymisen sovelluksesi liiketoimintalogiikkaan sen sijaan, että huolehtisit tiedostonhallinnan yksityiskohdista.
