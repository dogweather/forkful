---
title:                "Att arbeta med yaml"
html_title:           "Swift: Att arbeta med yaml"
simple_title:         "Att arbeta med yaml"
programming_language: "Swift"
category:             "Swift"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/swift/working-with-yaml.md"
---

{{< edit_this_page >}}

## Varför

Att arbeta med YAML kan ge dig möjlighet att enkelt och effektivt strukturera och organisera data. Det är särskilt användbart för hantering av konfigurationsfiler och utbytt av data mellan olika applikationer.

## Så här

För att börja arbeta med YAML i Swift behöver du först importera YAML-biblioteket. Sedan kan du använda kodexempel som följande för att läsa och skriva YAML-filer:

```Swift
// Importera YAML-biblioteket
import Yams

// Läs in en YAML-fil
let yamlString = """
name: John Smith
age: 30
"""

do {
    let yamlData = try Yams.load(yaml: yamlString)
    print(yamlData)
} catch {
    print(error)
}

// Skapa en dictionary för YAML-data
let yamlDictionary: [String: Any] = [
    "name": "Jane Doe",
    "age": 25
]

// Konvertera till YAML-format
do {
    let yamlData = try Yams.dump(object: yamlDictionary)
    print(yamlData)
} catch {
    print(error)
}
```

Output för läsningen av YAML-filen kommer att vara en `Any`-typ, medan output för konverteringen till YAML-format kommer att vara en `String`.

## Deep Dive

YAML är en strukturerad datastruktur som är lätt att läsa för både människor och datorer. Den använder enkel syntax som består av nycklar och värden som kan vara antingen enkla värden som strängar eller numeriska värden, eller mer komplexa som listor eller nested dictionaries.

Yaml-biblioteket i Swift är baserat på libyaml som ger en snabb och pålitlig YAML-parsing. Det är tillgängligt för både macOS och iOS.

## Se även

Här är några länkar som kan vara relevanta för dig om du är intresserad av att arbeta med YAML och Swift:

- [LibYAML](https://github.com/yaml/libyaml): Yaml-biblioteket som Yams är baserat på
- [Yaml.org](https://yaml.org/): Officiell hemsida för YAML
- [Yams dokumentation](https://github.com/jpsim/Yams/blob/master/Documentation/README.md): Dokumentation för Yams-biblioteket i Swift