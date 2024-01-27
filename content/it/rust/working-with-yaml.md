---
title:                "Lavorare con YAML"
date:                  2024-01-19
html_title:           "Bash: Lavorare con YAML"
simple_title:         "Lavorare con YAML"
programming_language: "Rust"
category:             "Rust"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/rust/working-with-yaml.md"
---

{{< edit_this_page >}}

## What & Why?
Lavorare con YAML significa manipolare dati strutturati facilmente leggibili dall'uomo. I programmatori lo fanno per configurazioni, serializzazione di dati o per interfacciarsi con servizi che lo adottano come Kubernetes o CI/CD tools.

## How to:
In Rust, per lavorare con YAML usiamo la crate `serde_yaml`. Ecco come serializzare e deserializzare dati:

```rust
use serde::{Serialize, Deserialize};
use serde_yaml;

#[derive(Debug, PartialEq, Serialize, Deserialize)]
struct Config {
    version: String,
    features: Vec<String>,
}

fn main() -> Result<(), serde_yaml::Error> {
    let data = "
        version: '1.0'
        features: 
            - fast
            - reliable
            - secure
    ";

    // Deserializzazione di una stringa YAML in un'istanza di Config
    let cfg: Config = serde_yaml::from_str(data)?;
    println!("{:?}", cfg);

    // Serializzazione di un'istanza di Config in una stringa YAML
    let serialized = serde_yaml::to_string(&cfg)?;
    println!("{}", serialized);
    
    Ok(())
}
```
Output di deserializzazione: `Config { version: "1.0", features: ["fast", "reliable", "secure"] }`
Output di serializzazione:
```yaml
---
version: "1.0"
features:
  - "fast"
  - "reliable"
  - "secure"
```

## Deep Dive
YAML, nato all'inizio degli anni 2000, è spesso preferito per la sua leggibilità rispetto a formati come JSON o XML. In Rust, l'ecosistema Serde fornisce strumenti per interagire con vari formati di serializzazione. Alternativamente, ci sono anche altre crate come `yaml-rust`, che non si appoggiano su Serde. Dettagli: `serde_yaml` usa `serde` per la serializzazione dei dati e `linked-hash-map` per mantenere l’ordine delle chiavi del YAML.

## See Also
- Documentazione di `serde_yaml`: https://docs.rs/serde_yaml/latest/serde_yaml/
- Serde Guida Ufficiale: https://serde.rs/
- YAML Spec: https://yaml.org/spec/1.2/spec.html
- Comparazione delle Crate YAML per Rust: https://lib.rs/crates/categories/encoding_format::yaml
