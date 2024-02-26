---
date: 2024-01-26 04:26:43.643891-07:00
description: "TOML (Toms Opplagte, Minimalistiske Spr\xE5k) er et data serialiseringsformat\
  \ som er lett \xE5 lese p\xE5 grunn av dets klare semantikk. Programmerere bruker\
  \ TOML\u2026"
lastmod: '2024-02-25T18:49:39.344153-07:00'
model: gpt-4-0125-preview
summary: "TOML (Toms Opplagte, Minimalistiske Spr\xE5k) er et data serialiseringsformat\
  \ som er lett \xE5 lese p\xE5 grunn av dets klare semantikk. Programmerere bruker\
  \ TOML\u2026"
title: Jobbe med TOML
---

{{< edit_this_page >}}

## Hva & hvorfor?
TOML (Toms Opplagte, Minimalistiske Språk) er et data serialiseringsformat som er lett å lese på grunn av dets klare semantikk. Programmerere bruker TOML til konfigurasjonsfiler hvor lesbarhet for mennesker og enkel tolkning for maskiner er nøkkelen.

## Hvordan:
For å starte trenger du en TOML-tolker. Swift har ikke en innebygd, så la oss bruke `TOMLDecoder`. Installer den via Swift Package Manager og deretter serialiser og deserialiser TOML med letthet.

```Swift
import TOMLDecoder

let tomlString = """
title = "TOML Eksempel"

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
        print("Tittel: \(config.title), Eier: \(config.owner.name), Fødselsdato: \(config.owner.dob)")
    } catch {
        print("Feil under tolking av TOML: \(error)")
    }
}
```

Denne koden gir følgende utskrift:
```
Tittel: TOML Eksempel, Eier: Tom Preston-Werner, Fødselsdato: 1979-05-27 07:32:00 +0000
```

## Dypdykk
TOML ble designet av Tom Preston-Werner, medgrunnleggeren av GitHub, som et mer menneskevennlig alternativ til formater som JSON eller YAML. Det sikter mot klarhet, og reduserer sjansene for feiltolkning av et menneske eller maskin. Når det gjelder alternativer, er YAML og JSON de vanlige mistenkte, med YAML vinklet mot menneskelig lesbarhet og JSON som det enklere maskinvennlige alternativet. Når vi arbeider med TOML i Swift, har vi ikke en innfødt parser. Imidlertid letter tredjepartsbiblioteker som `TOMLDecoder` enkel konvertering mellom TOML-strenger og Swift-typer, spesifikt via `Codable`-protokoller introdusert i Swift 4 som strømlinjeformet serialisering.

## Se også
- TOML-standarden: https://toml.io
- GitHub for `TOMLDecoder`: https://github.com/dduan/TOMLDecoder
- Swift-dokumentasjon på `Codable`: https://developer.apple.com/documentation/swift/codable
- Sammenligning av data serialiseringsformater: https://en.wikipedia.org/wiki/Comparison_of_data-serialization_formats
