---
title:                "Swift: Å jobbe med yaml"
simple_title:         "Å jobbe med yaml"
programming_language: "Swift"
category:             "Swift"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/swift/working-with-yaml.md"
---

{{< edit_this_page >}}

## Hvorfor

I denne bloggposten skal vi se nærmere på hvordan man kan jobbe med YAML i Swift-programmering. YAML, eller YAML Ain't Markup Language, er et strukturert filformat som er brukt til å beskrive data på en enkel og lesbar måte. Det er spesielt nyttig i utviklingen av større og komplekse programmer, da det gjør det enklere å håndtere store mengder data.

## Slik gjør du det

For å kunne jobbe med YAML i Swift, må du først installere et tredjepartsbibliotek kalt "Yams". Dette kan enkelt gjøres ved å legge til følgende linje i dependencies i Package.swift-filen:

```Swift
.package(url: "https://github.com/jpsim/Yams.git", from: "4.0.0")
```

Deretter må du importere Yams i den filen du ønsker å jobbe med YAML i:

```Swift
import Yams
```

For å konvertere YAML-data til Swift-objekter, kan du bruke funksjonen `load(yaml:)` fra Yams-biblioteket:

```Swift
let yamlString = """
myKey: myValue
myArray:
  - value1
  - value2
"""

do {
    let yamlObject = try load(yaml: yamlString)
    print(yamlObject)
} catch {
    print("Feil ved lasting av YAML-data: \(error)")
}
```

Outputen vil bli en Swift Dictionary som inneholder nøkler og verdier som tilsvarer YAML-dataen:

```Swift
["myKey": "myValue", "myArray": ["value1", "value2"]]
```

Du kan også konvertere Swift-objekter til YAML-data ved å bruke funksjonen `dump` fra Yams-biblioteket:

```Swift
let myDict = ["myKey": "myValue", "myArray": ["value1", "value2"]]

do {
    let yamlData = try dump(object: myDict)
    print(yamlData)
} catch {
    print("Feil ved dumping av YAML-data: \(error)")
}
```

Outputen vil bli en streng som inneholder YAML-formatert data:

```Swift
myKey: myValue
myArray:
  - value1
  - value2
```

## Dypdykk

Yams-biblioteket støtter også mer avansert håndtering av YAML-data, som for eksempel å opprette egne typer som kan serialiseres til og deserialiseres fra YAML-data. Du kan lese mer om dette i dokumentasjonen til Yams-biblioteket.

## Se også

- Yams dokumentasjon: https://github.com/jpsim/Yams
- Swift Package Manager dokumentasjon: https://swift.org/package-manager/