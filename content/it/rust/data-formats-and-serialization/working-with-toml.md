---
title:                "Lavorare con TOML"
date:                  2024-01-26T04:26:15.269322-07:00
model:                 gpt-4-0125-preview
simple_title:         "Lavorare con TOML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/rust/working-with-toml.md"
---

{{< edit_this_page >}}

## Cosa & Perché?
TOML è un linguaggio di serializzazione dati leggibile dall'uomo, spesso utilizzato per le configurazioni. I programmatori usano TOML per la sua semplicità e chiarezza, che si traduce facilmente in una hash map in Rust.

## Come fare:
```Rust
// 1. Includi il crate 'toml' nel tuo Cargo.toml
// [dipendenze]
// toml = "0.5"

// 2. Deserializza TOML in una struct in Rust
use toml::Value;

fn main() {
    let contenuto_toml = r#"
        [server]
        host = "localhost"
        porta = 8080
    "#;

    let valore = contenuto_toml.parse::<Value>().unwrap();
    let host = valore.get("server").unwrap().get("host").unwrap();
    let porta = valore.get("server").unwrap().get("porta").unwrap();
    
    println!("Il server è attivo su {}:{}", host, porta);
    // Output: Il server è attivo su "localhost":8080
}
```

## Approfondimento
TOML, che sta per Tom's Obvious, Minimal Language, è stato creato da Tom Preston-Werner nel 2013. Il suo obiettivo è essere più leggibile di JSON o YAML per i file di configurazione. Il design di TOML si concentra su una sintassi univoca, minimalismo e mappatura semplice sui tipi di dati.

Le alternative a TOML includono JSON, YAML e XML, ma TOML prevale in scenari dove la leggibilità umana e la modifica dei file da parte di non programmatori è cruciale. Quando si lavora con TOML in Rust, serde offre una solida base per la serializzazione e deserializzazione, usando traits per mappare TOML sulle struct di Rust senza sforzo.

Una sfida nell'utilizzo di TOML è la sua rigidezza sui tipi e sulla struttura. Il programmatore deve definire un sistema di tipi Rust ben strutturato che rifletta lo schema dei dati TOML per utilizzare efficacemente TOML in Rust.

## Vedi Anche
- [Documentazione TOML](https://toml.io/en/)
- [Crate serde_toml](https://docs.rs/serde_toml/)
- [Libro del Linguaggio di Programmazione Rust](https://doc.rust-lang.org/stable/book/)
- [Repo GitHub di TOML](https://github.com/toml-lang/toml)
