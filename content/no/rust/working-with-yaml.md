---
title:                "Å jobbe med yaml"
html_title:           "Rust: Å jobbe med yaml"
simple_title:         "Å jobbe med yaml"
programming_language: "Rust"
category:             "Rust"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/rust/working-with-yaml.md"
---

{{< edit_this_page >}}

# Hva & Hvorfor?
YAML står for "YAML Ain't Markup Language" og er et tekstbasert språk som brukes til å representere datastrukturer. Programmerere bruker YAML for å lagre og overføre data på en enkel og menneskeleselig måte.

# Slik gjør du det:
```Rust
use serde_yaml; // importerer biblioteket

// opprett en struktur som representerer dataen din
#[derive(Debug, Serialize, Deserialize)]
struct Person {
    name: String,
    age: u8,
    hobbies: Vec<String>,
}

let data = "
    name: John
    age: 25
    hobbies:
        - Reading
        - Hiking
        - Cooking
"; // her kan du legge inn dataen du vil representere i YAML-format

// konverter dataen til YAML-format
let yaml = serde_yaml::to_string(&data)?;

// konverter YAML tilbake til datastruktur
let person: Person = serde_yaml::from_str(&yaml)?;
println!("{:?}", person); // resultat: Person { name: "John", age: 25, hobbies: ["Reading", "Hiking", "Cooking"] }
```

# Dypdykk:
YAML ble utviklet i 2001 av Clark Evans som en enklere og mer lesbar alternativ til XML og JSON. Det er et populært valg for konfigurasjonsfiler og brukes også i mange programmeringsspråk som Python, Java og selvfølgelig Rust.

I Rust finnes det også alternativer for å jobbe med datastrukturer som Toml, som er mer fokusert på konfigurasjonsfiler, og JSON, som er mer utbredt i webutvikling. Men YAML tilbyr en fin balanse mellom lesbarhet og funksjonalitet, og det er enkelt å integrere med biblioteker som serde_yaml.

# Se også:
- [YAML-spesifikasjonen](https://yaml.org/spec/)
- [Sammenligning av YAML, JSON og Toml](https://stackshare.io/stackups/json-vs-toml-vs-yaml)