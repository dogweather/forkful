---
title:                "Arbeta med yaml"
html_title:           "Rust: Arbeta med yaml"
simple_title:         "Arbeta med yaml"
programming_language: "Rust"
category:             "Rust"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/rust/working-with-yaml.md"
---

{{< edit_this_page >}}

## Varför

Som programmerare är det ständigt viktigt att hantera data på ett strukturerat sätt. Även om det finns flera alternativ för att spara och hantera data, har YAML blivit ett populärt val på grund av dess enkelhet och läsbarhet. Med YAML kan du snabbt och effektivt organisera dina data och integrera det med andra språk och verktyg.

## Hur man använder YAML i Rust

För att använda YAML i ditt Rust-projekt måste du först lägga till YAML-paketet i din `Cargo.tomls` fil:

```Rust
[dependencies]
yaml-rust = "0.4.3"
```

Sedan kan du importera YAML-paketet och använda det för att läsa och skriva YAML-filer:

```Rust
extern crate yaml_rust;

use std::fs;
use yaml_rust::{YamlLoader, YamlEmitter};

fn main() {
    // Läs in en YAML-fil
    let yaml = fs::read_to_string("data.yml").unwrap();

    // Konvertera YAML-strängen till en Yaml-object
    let docs = YamlLoader::load_from_str(&yaml).unwrap();

    // Skriv ut Yaml-objektet
    println!("{:#?}", docs);

    // Skapa en ny Yaml-objekt och lägg till data
    let mut data = yaml_rust::Yaml::Hash(HashMap::new());
    data["name"] = yaml_rust::Yaml::String("John Doe".to_string());
    data["age"] = yaml_rust::Yaml::Integer(25);

    // Skapa en YamlEmitter för att skriva ut YAML
    let mut emitter = YamlEmitter::new(&mut buffer);

    // Skriv Yaml-objektet till en YAML-fil
    emitter.dump(&data).unwrap()
}
```

När du kör programmet ovan kommer det att läsa in YAML-filen `data.yml`, konvertera den till en Yaml-objekt och skriva ut den i terminalen. Sedan skapas en ny Yaml-objekt och läggs till data innan det skrivs ut till en ny YAML-fil med hjälp av YamlEmitter.

## Djupdykning

Yaml är baserat på en enkel nyckel-värde-parsyntax, vilket gör det lätt att läsa och skriva. Det stöder både listor och dictionary-strukturer, vilket gör det flexibelt för att spara olika typer av data. Dessutom är YAML-kod portabelt och kan läsas av många olika programmeringsspråk.

YAML i Rust har också stöd för att hantera komplexa datastrukturer med hjälp av `Yaml::Hash` och `Yaml::Array` typer, vilket gör det möjligt att skapa djupare hierarkier av data. Det finns också flera funktioner som gör det möjligt att enkelt söka och manipulera YAML-data.

## Se också

- YAML officiell hemsida (https://yaml.org/)
- YAML Guide (https://yaml.org/start.html)
- Rust YAML-dokumentation (https://docs.rs/yaml-rust/0.4.3/yaml_rust/)