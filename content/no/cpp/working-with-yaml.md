---
title:                "Å jobbe med yaml"
html_title:           "C++: Å jobbe med yaml"
simple_title:         "Å jobbe med yaml"
programming_language: "C++"
category:             "C++"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/cpp/working-with-yaml.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
YAML er en populær måte å representere og lagre data på i programmering. Det brukes ofte som et enkelt og menneskelesbart alternativ til JSON-formatet. Programmører bruker YAML for å organisere og strukturere data på en oversiktlig måte.

## Hvordan:
Slik ser en enkel YAML-fil ut:
```
name: John Doe
age: 30
hobbies:
- coding
- reading
```
For å lese denne filen i C++ trenger vi et bibliotek som støtter YAML. Et populært alternativ er [yaml-cpp](https://github.com/jbeder/yaml-cpp). Vi kan deretter lese filen og få tilgang til dataene slik:
```
YAML::Node data = YAML::LoadFile("file.yaml");

std::string name = data["name"].as<std::string>();
int age = data["age"].as<int>();
std::vector<std::string> hobbies = data["hobbies"].as<std::vector<std::string>>();
```

## Dypdykk:
YAML ble opprinnelig laget i 2001 av Clark Evans og Ingy döt Net. Det er inspirert av XML, men med fokus på lesbarhet og enklere syntaks. Alternativer til YAML inkluderer JSON og XML, men YAML er ofte foretrukket på grunn av sin menneskelesbare formatering.

Når du jobber med YAML, kan det være nyttig å vite at nøkler (keys) er case-sensitiv og at tegn som `:` og `-` har en spesiell betydning i syntaksen. Du kan også bruke ankre (`&`) og referanser (`*`) for å unngå å gjenta data.

## Se også:
- [YAML offisiell nettside](https://yaml.org/)
- [YAML forstått som det er ment å være forstått](https://www.yaml.info/)