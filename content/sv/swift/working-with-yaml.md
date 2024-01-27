---
title:                "Arbete med YAML"
date:                  2024-01-19
html_title:           "Arduino: Arbete med YAML"
simple_title:         "Arbete med YAML"
programming_language: "Swift"
category:             "Swift"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/swift/working-with-yaml.md"
---

{{< edit_this_page >}}

## Vad & Varför?

YAML står för "YAML Ain't Markup Language" och används ofta för att konfigurera programvara och tjänster. Programutvecklare väljer YAML på grund av dess läsbarhet och enkelhet att mappa mot datatyper som finns i många programmeringsspråk.

## How to:

För att jobba med YAML i Swift behöver du ett bibliotek som `Yams`. Se till att du har bundit det i din `Package.swift`-fil.

```Swift
// I filen Package.swift, lägg till 
dependencies: [
    .package(url: "https://github.com/jpsim/Yams.git", .upToNextMajor(from: "4.0.0"))
],
targets: [
    .target(
        name: "DittProjekt",
        dependencies: ["Yams"]),
]
```

Använd sedan Yams för att tolka YAML-strängar.

```Swift
import Yams

let yamlStr = """
- name: Marvin
  job: Paranoid Android
- name: Zaphod
  job: Ex-Galactic President
"""

do {
    let people = try Yams.load(yaml: yamlStr)
    print(people)
} catch {
    print("Failed to parse YAML: \(error)")
}
```

Förväntad utskrift skulle vara en lista av dictionary-medlemmar som representerar data som YAML-strängen innehåller.

## Deep Dive:

YAML skapades 2001 och är en superset till JSON, vilket innebär att all JSON är giltig YAML. Alternativ till YAML inkluderar JSON och XML, men YAML är ofta föredraget för dess human-läsbarhet. I Swift, åstadkoms YAML-parsing implementeras genom att använda en `Decoder` som omvandlar text till Swift datatyper.

## See Also:

- YAML-specifikationen: [https://yaml.org/spec/1.2/spec.html](https://yaml.org/spec/1.2/spec.html)
- Yams GitHub-repositorium: [https://github.com/jpsim/Yams](https://github.com/jpsim/Yams)
- Swift Package Manager dokumentation: [https://swift.org/package-manager/](https://swift.org/package-manager/)
