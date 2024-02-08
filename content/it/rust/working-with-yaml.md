---
title:                "Lavorare con YAML"
aliases:
- it/rust/working-with-yaml.md
date:                  2024-02-03T19:26:59.483853-07:00
model:                 gpt-4-0125-preview
simple_title:         "Lavorare con YAML"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/rust/working-with-yaml.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cos'è & Perché?

Nella programmazione in Rust, lavorare con YAML (YAML Ain't Markup Language) significa effettuare il parsing e la generazione di dati in formato YAML, uno standard di serializzazione dei dati amichevole per l'utente. I programmatori integrano la gestione di YAML in Rust per configurare applicazioni, gestire impostazioni o elaborare strutture di dati complesse in un formato chiaro e leggibile, sfruttando la sua semplicità rispetto a JSON o XML per file di configurazione e scambio di dati.

## Come fare:

Rust non supporta YAML nella sua libreria standard, quindi comunemente si utilizzano crate di terze parti come `serde` (per serializzare e deserializzare dati) in combinazione con `serde_yaml`.

Prima, aggiungi le dipendenze al tuo `Cargo.toml`:

```toml
[dependencies]
serde = { version = "1.0", features = ["derive"] }
serde_yaml = "0.8"
```

Ora, vediamo come deserializzare una stringa YAML in una struct Rust e serializzare una struct Rust di nuovo in una stringa YAML.

### Deserializzare YAML in Strutture Rust

Definisci una struct Rust che rifletta i dati che ti aspetti in YAML. Usa gli attributi di Serde per la personalizzazione, se necessario.

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
name: Scudo
durability: 300
owner:
  name: Steve
  age: 25
";

    let config_deserializzato: Config = serde_yaml::from_str(yaml_data).unwrap();
    println!("{:?}", config_deserializzato);
}
```

L'output di esempio dopo aver eseguito il codice Rust sopra sarà:

```plaintext
Config { name: "Scudo", durability: 300, owner: Owner { name: "Steve", age: 25 } }
```

### Serializzare Strutture Rust in YAML

Questo esempio prende la struct `Config` della sezione precedente e la serializza di nuovo in formato YAML.

```rust
fn main() {
    let config = Config {
        name: String::from("Ascia"),
        durability: 120,
        owner: Owner {
            name: String::from("Alex"),
            age: 30,
        },
    };

    let yaml_serializzato = serde_yaml::to_string(&config).unwrap();
    println!("{}", yaml_serializzato);
}
```

L'output atteso sarà una stringa formattata in YAML:

```yaml
---
name: Ascia
durability: 120
owner:
  name: Alex
  age: 30
```

Questi frammenti dimostrano come integrare efficacemente il parsing e la generazione di YAML nelle tue applicazioni Rust, utilizzando i popolari crate `serde` e `serde_yaml`, gestendo strutture di dati complesse e fornendo configurazioni semplici e leggibili per l'uomo.
