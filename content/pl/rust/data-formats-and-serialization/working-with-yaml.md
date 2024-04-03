---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:26:56.160845-07:00
description: "W programowaniu w Rust, praca z YAML (YAML Ain't Markup Language) polega\
  \ na analizowaniu i generowaniu danych w formacie YAML, czyli przyjaznym dla\u2026"
lastmod: '2024-03-13T22:44:35.206706-06:00'
model: gpt-4-0125-preview
summary: "W programowaniu w Rust, praca z YAML (YAML Ain't Markup Language) polega\
  \ na analizowaniu i generowaniu danych w formacie YAML, czyli przyjaznym dla cz\u0142\
  owieka standardzie serializacji danych."
title: Praca z YAML
weight: 41
---

## Co i dlaczego?

W programowaniu w Rust, praca z YAML (YAML Ain't Markup Language) polega na analizowaniu i generowaniu danych w formacie YAML, czyli przyjaznym dla człowieka standardzie serializacji danych. Programiści integrują obsługę YAML w Rust, aby konfigurować aplikacje, zarządzać ustawieniami lub przetwarzać złożone struktury danych w przejrzystym i czytelnym formacie, wykorzystując jego prostotę w porównaniu do JSON lub XML dla plików konfiguracyjnych i wymiany danych.

## Jak to zrobić:

Rust nie obsługuje YAML w swojej bibliotece standardowej, dlatego powszechnie używamy zewnętrznych skrzyńek, takich jak `serde` (do serializacji i deserializacji danych) w połączeniu z `serde_yaml`.

Najpierw dodaj zależności do swojego `Cargo.toml`:

```toml
[dependencies]
serde = { version = "1.0", features = ["derive"] }
serde_yaml = "0.8"
```

Teraz zobaczmy, jak deserializować ciąg YAML do struktury Rust i serializować strukturę Rust z powrotem do ciągu YAML.

### Deserializacja YAML do struktur Rust

Zdefiniuj strukturę Rust, która odzwierciedla dane, jakich oczekujesz w YAML. Użyj atrybutów Serde do dostosowania, jeśli jest to potrzebne.

```rust
use serde::{Deserialize, Serialize};
use serde_yaml;

#[derive(Debug, PartialEq, Serialize, Deserialize)]
struct Config {
    name: String,
    durability: i32,
    owner: Owner,
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
struct Owner {
    name: String,
    age: i32,
}

fn main() {
    let yaml_data = "
name: Shield
durability: 300
owner:
  name: Steve
  age: 25
";

    let deserialized_config: Config = serde_yaml::from_str(yaml_data).unwrap();
    println!("{:?}", deserialized_config);
}
```

Przykładowe wyjście po uruchomieniu powyższego kodu Rust będzie takie:

```plaintext
Config { name: "Shield", durability: 300, owner: Owner { name: "Steve", age: 25 } }
```

### Serializacja struktur Rust do YAML

Ten przykład bierze strukturę `Config` z poprzedniej sekcji i serializuje ją z powrotem do formatu YAML.

```rust
fn main() {
    let config = Config {
        name: String::from("Axe"),
        durability: 120,
        owner: Owner {
            name: String::from("Alex"),
            age: 30,
        },
    };

    let serialized_yaml = serde_yaml::to_string(&config).unwrap();
    println!("{}", serialized_yaml);
}
```

Oczekiwane wyjście będzie ciągiem sformatowanym w YAML:

```yaml
---
name: Axe
durability: 120
owner:
  name: Alex
  age: 30
```

Te fragmenty kodu demonstrują, jak efektywnie zintegrować parsowanie i generowanie YAML w twoich aplikacjach Rust, używając popularnych skrzynek `serde` i `serde_yaml`, umożliwiając obsługę złożonych struktur danych i zapewniając proste, czytelne konfiguracje.
