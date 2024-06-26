---
date: 2024-01-26 04:26:32.389417-07:00
description: "Kuinka: Aloittaaksesi tarvitset TOML-j\xE4sent\xE4j\xE4n. Swiftill\xE4\
  \ ei ole sis\xE4\xE4nrakennettua, joten k\xE4ytet\xE4\xE4n `TOMLDecoder`ia. Asenna\
  \ se Swift Package Managerin\u2026"
lastmod: '2024-03-13T22:44:56.930954-06:00'
model: gpt-4-0125-preview
summary: "Aloittaaksesi tarvitset TOML-j\xE4sent\xE4j\xE4n."
title: "Ty\xF6skentely TOML:n kanssa"
weight: 39
---

## Kuinka:
Aloittaaksesi tarvitset TOML-jäsentäjän. Swiftillä ei ole sisäänrakennettua, joten käytetään `TOMLDecoder`ia. Asenna se Swift Package Managerin kautta ja sen jälkeen serialisoi ja deserialisoi TOML helposti.

```Swift
import TOMLDecoder

let tomlString = """
title = "TOML-esimerkki"

[owner]
name = "Tom Preston-Werner"
dob = 1979-05-27T07:32:00Z
"""

struct Config: Codable {
    let title: String
    let owner: Owner
}

struct Owner: Codable {
    let name: String
    let dob: Date
}

let decoder = TOMLDecoder()
if let configData = tomlString.data(using: .utf8) {
    do {
        let config = try decoder.decode(Config.self, from: configData)
        print("Otsikko: \(config.title), Omistaja: \(config.owner.name), Syntymäaika: \(config.owner.dob)")
    } catch {
        print("Virhe TOML:n jäsentämisessä: \(error)")
    }
}
```

Tämä koodi tuottaa tulosteen:
```
Otsikko: TOML-esimerkki, Omistaja: Tom Preston-Werner, Syntymäaika: 1979-05-27 07:32:00 +0000
```

## Syväsukellus
TOML on suunniteltu Tom Preston-Wernerin, GitHubin perustajajäsenen, toimesta ihmisläheisemmäksi vaihtoehdoksi formaateille kuten JSON tai YAML. Se pyrkii selkeyteen, vähentämään väärinkäsitysten mahdollisuuksia ihmisen tai koneen osalta. Vaihtoehtoina ovat tavallisesti YAML ja JSON, joista YAML on kallistunut ihmisen luettavuuteen ja JSON yksinkertaisempaan koneystävällisyyteen. Työskennellessä TOML:n kanssa Swiftissä, meillä ei ole natiivia jäsentäjää. Kuitenkin, kolmannen osapuolen kirjastot kuten `TOMLDecoder` helpottavat TOML-merkkijonojen ja Swift-tyyppien välistä muunnosta, erityisesti `Codable`-protokollien kautta, jotka otettiin käyttöön Swift 4:ssä ja jotka tehostivat serialisointia.

## Katso Myös
- TOML-standardi: https://toml.io
- GitHub `TOMLDecoder`ille: https://github.com/dduan/TOMLDecoder
- Swift-dokumentaatio `Codable`sta: https://developer.apple.com/documentation/swift/codable
- Data-serialisointiformaattien vertailu: https://fi.wikipedia.org/wiki/Vertailu_data-serialisointiformaatteista
