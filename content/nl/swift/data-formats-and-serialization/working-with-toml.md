---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:10:59.170587-07:00
description: "TOML (Tom's Obvious, Minimal Language) is een gegevensserialisatieformaat\
  \ dat makkelijk te lezen is vanwege de duidelijke semantiek. Programmeurs\u2026"
lastmod: '2024-03-11T00:14:25.019798-06:00'
model: gpt-4-0125-preview
summary: "TOML (Tom's Obvious, Minimal Language) is een gegevensserialisatieformaat\
  \ dat makkelijk te lezen is vanwege de duidelijke semantiek. Programmeurs\u2026"
title: Werken met TOML
---

{{< edit_this_page >}}

## Wat & Waarom?
TOML (Tom's Obvious, Minimal Language) is een gegevensserialisatieformaat dat makkelijk te lezen is vanwege de duidelijke semantiek. Programmeurs gebruiken TOML voor configuratiebestanden waarbij leesbaarheid door mensen en eenvoudig parsen door machines sleutel zijn.

## Hoe te:
Om te beginnen, heb je een TOML-parser nodig. Swift heeft geen ingebouwde, dus laten we `TOMLDecoder` gebruiken. Installeer het via Swift Package Manager en serializeer en deserializeer vervolgens TOML met gemak.

```Swift
import TOMLDecoder

let tomlString = """
title = "Voorbeeld van TOML"

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
        print("Titel: \(config.title), Eigenaar: \(config.owner.name), Geb.dat.: \(config.owner.dob)")
    } catch {
        print("Fout bij het parsen van TOML: \(error)")
    }
}
```

Deze code geeft uit:
```
Titel: Voorbeeld van TOML, Eigenaar: Tom Preston-Werner, Geb.dat.: 1979-05-27 07:32:00 +0000
```

## Diepgaande Duik
TOML is ontworpen door Tom Preston-Werner, de medeoprichter van GitHub, als een meer mensvriendelijk alternatief voor formaten zoals JSON of YAML. Het streeft naar helderheid, waardoor de kans op misinterpretatie door een mens of machine wordt verminderd. Wat betreft alternatieven, zijn YAML en JSON de gebruikelijke verdachten, met YAML gericht op menselijke leesbaarheid en JSON als de eenvoudigere machinevriendelijke optie. Bij het werken met TOML in Swift, hebben we geen native parser. Echter, bibliotheken van derden zoals `TOMLDecoder` vergemakkelijken de eenvoudige omzetting tussen TOML-strings en Swift-typen, specifiek via `Codable` protocollen die in Swift 4 zijn geïntroduceerd en die serialisatie hebben gestroomlijnd.

## Zie Ook
- De TOML-standaard: https://toml.io
- GitHub voor `TOMLDecoder`: https://github.com/dduan/TOMLDecoder
- Swift Documentatie over `Codable`: https://developer.apple.com/documentation/swift/codable
- Vergelijking van gegevensserialisatieformaten: https://nl.wikipedia.org/wiki/Vergelijking_van_gegevensserialisatieformaten
