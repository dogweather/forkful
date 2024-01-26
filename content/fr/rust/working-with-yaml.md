---
title:                "Travailler avec YAML"
html_title:           "Bash: Travailler avec YAML"
simple_title:         "Travailler avec YAML"
programming_language: "Rust"
category:             "Rust"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/rust/working-with-yaml.md"
---

{{< edit_this_page >}}

## What & Why?
YAML, c'est un format de données facile à lire pour l'humain. Les programmeurs s'en servent pour la configuration, la sérialisation de données, et parce que c'est plus clean et moins verbeux que JSON ou XML.

## How to:
Installez la crate `serde_yaml` pour la sérialisation et désérialisation avec YAML. Voici comment:

```Rust
use serde::{Serialize, Deserialize};
use serde_yaml;

#[derive(Debug, Serialize, Deserialize)]
struct Config {
    version: String,
    deploy: bool,
}

fn main() -> serde_yaml::Result<()> {
    let data = r#"
        version: '1.0.0'
        deploy: true
    "#;

    let config: Config = serde_yaml::from_str(data)?;
    println!("{:?}", config);

    // Sérialisation
    let serialized = serde_yaml::to_string(&config)?;
    println!("{}", serialized);

    Ok(())
}
```

Sortie:
```
Config { version: "1.0.0", deploy: true }
version: "1.0.0"
deploy: true
```

## Deep Dive
YAML existe depuis 2001, conçu pour être facilement lisible. Alternatives? JSON pour les APIs, XML quand on n'a pas le choix. Rust avec `serde_yaml` utilise `serde`, un cadre de sérialisation générique, assurant performance et flexibilité.

## See Also
- YAML officiel: https://yaml.org
- Crates `serde` et `serde_yaml`: https://crates.io/crates/serde, https://crates.io/crates/serde_yaml
- Guide Serde: https://serde.rs
