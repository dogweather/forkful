---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:24:29.755273-07:00
description: "Comment faire : Pour travailler avec le JSON en Rust, on utilise largement\
  \ le crate `serde` ainsi que `serde_json` pour la s\xE9rialisation et la\u2026"
lastmod: '2024-03-13T22:44:57.518018-06:00'
model: gpt-4-0125-preview
summary: "Pour travailler avec le JSON en Rust, on utilise largement le crate `serde`\
  \ ainsi que `serde_json` pour la s\xE9rialisation et la d\xE9s\xE9rialisation."
title: Travailler avec JSON
weight: 38
---

## Comment faire :
Pour travailler avec le JSON en Rust, on utilise largement le crate `serde` ainsi que `serde_json` pour la sérialisation et la désérialisation. Tout d'abord, assurez-vous de les inclure dans votre `Cargo.toml` :

```toml
[dependencies]
serde = { version = "1.0", features = ["derive"] }
serde_json = "1.0"
```

### Exemple 1 : Désérialiser du JSON vers une structure Rust
Définissez une structure Rust et utilisez les macros derive pour `Deserialize` et `Serialize` :

```rust
use serde::{Deserialize, Serialize};

#[derive(Serialize, Deserialize)]
struct Utilisateur {
    id: u32,
    nom: String,
    email: String,
}

fn main() {
    let donnees_json = r#"
        {
            "id": 1,
            "nom": "Jane Doe",
            "email": "jane.doe@example.com"
        }
    "#;

    let utilisateur: Utilisateur = serde_json::from_str(donnees_json).unwrap();

    println!("ID Utilisateur : {}", utilisateur.id);
    println!("Nom Utilisateur : {}", utilisateur.nom);
    println!("Email Utilisateur : {}", utilisateur.email);
}
```

**Sortie :**

```
ID Utilisateur : 1
Nom Utilisateur : Jane Doe
Email Utilisateur : jane.doe@example.com
```

### Exemple 2 : Sérialiser une structure Rust en JSON
En utilisant la même structure `Utilisateur` :

```rust
let utilisateur = Utilisateur {
    id: 1,
    nom: "Jane Doe".to_string(),
    email: "jane.doe@example.com".to_string(),
};

let donnees_json = serde_json::to_string(&utilisateur).unwrap();

println!("{}", donnees_json);
```

**Sortie :**

```json
{"id":1,"nom":"Jane Doe","email":"jane.doe@example.com"}
```

Ces exemples démontrent le processus de base pour désérialiser le JSON en structures Rust et pour sérialiser les structures Rust en chaînes JSON. Serde offre un ensemble riche d'outils pour travailler avec le JSON, incluant la gestion des champs optionnels, l'imbrication complexe, et les types non directement supportés par JSON.
