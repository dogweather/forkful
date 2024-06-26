---
date: 2024-01-26 04:26:53.627027-07:00
description: "Hur man g\xF6r: F\xF6r att b\xF6rja beh\xF6ver du en TOML-tolk. Swift\
  \ har inte en inbyggd s\xE5dan, s\xE5 l\xE5t oss anv\xE4nda `TOMLDecoder`. Installera\
  \ den via Swift Package\u2026"
lastmod: '2024-03-13T22:44:38.274722-06:00'
model: gpt-4-0125-preview
summary: "F\xF6r att b\xF6rja beh\xF6ver du en TOML-tolk."
title: Att arbeta med TOML
weight: 39
---

## Hur man gör:
För att börja behöver du en TOML-tolk. Swift har inte en inbyggd sådan, så låt oss använda `TOMLDecoder`. Installera den via Swift Package Manager och sedan serialisera och deserialisera TOML med lätthet.

```Swift
import TOMLDecoder

let tomlString = """
title = "TOML-exempel"

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
        print("Titel: \(config.title), Ägare: \(config.owner.name), Födelsedag: \(config.owner.dob)")
    } catch {
        print("Fel vid tolkning av TOML: \(error)")
    }
}
```

Den här koden ger utdata:
```
Titel: TOML-exempel, Ägare: Tom Preston-Werner, Födelsedag: 1979-05-27 07:32:00 +0000
```

## Fördjupning
TOML designades av Tom Preston-Werner, medgrundare av GitHub, som ett mer människovänligt alternativ till format som JSON eller YAML. Det strävar efter tydlighet, vilket minskar chanserna för feltolkning av en människa eller maskin. När det gäller alternativ är YAML och JSON de vanliga misstänkta, med YAML lutat mot läsbarhet för människor och JSON som det enklare maskinvänliga alternativet. När vi arbetar med TOML i Swift har vi ingen inbyggd tolk. Dock underlättar tredjepartsbibliotek som `TOMLDecoder` enkel omvandling mellan TOML-strängar och Swift-typer, specifikt via `Codable`-protokoll introducerade i Swift 4 som strömlinjeformat serialisering.

## Se även
- TOML-standarden: https://toml.io
- GitHub för `TOMLDecoder`: https://github.com/dduan/TOMLDecoder
- Swift-dokumentation om `Codable`: https://developer.apple.com/documentation/swift/codable
- Jämförelse av data-serialiseringsformat: https://en.wikipedia.org/wiki/Comparison_of_data-serialization_formats
