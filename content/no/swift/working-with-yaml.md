---
title:                "Arbeide med yaml"
html_title:           "Swift: Arbeide med yaml"
simple_title:         "Arbeide med yaml"
programming_language: "Swift"
category:             "Swift"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/swift/working-with-yaml.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
YAML er et format for strukturert data som brukes til å representere informasjon på en leselig måte. Det blir ofte brukt av programmører for å konfigurere og lagre data, for eksempel i konfigurasjonsfiler for programvare.

## Hvordan:
```Swift
let yaml = """
person:
  name: John Doe
  age: 30
  occupation: Programmer
  languages:
    - Swift
    - Python
    - JavaScript
"""
if let person = try? Yaml.decode(yaml) as? [String: Any] {
  print(person["name"]) // John Doe
  print(person["age"]) // 30
  print(person["languages"]) // ["Swift", "Python", "JavaScript"]
}
```

## Dykk dypere:
YAML ble først introdusert i 2001 av Ingy döt Net, og blir stadig mer populært blant programmører på grunn av dets lettleselighet og fleksibilitet. Alternativer til YAML inkluderer JSON og XML, men YAML er vanligvis mer leselig og enklere å jobbe med. Implementeringen av YAML i Swift er gjort gjennom et tredjepartsbibliotek, som gjør det enkelt å integrere i eksisterende kode.

## Se også:
- [Offisiell YAML-nettside](https://yaml.org/)
- [Tredjepartsbibliotek for implementering av YAML i Swift](https://github.com/behrang/YamlSwift)