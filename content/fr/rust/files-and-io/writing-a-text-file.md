---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:29:19.384858-07:00
description: "\xC9crire un fichier texte en Rust implique de cr\xE9er, \xE9crire et\
  \ potentiellement ajouter des donn\xE9es \xE0 un fichier sur le syst\xE8me de fichiers.\
  \ Les programmeurs\u2026"
lastmod: '2024-03-13T22:44:57.511095-06:00'
model: gpt-4-0125-preview
summary: "\xC9crire un fichier texte en Rust implique de cr\xE9er, \xE9crire et potentiellement\
  \ ajouter des donn\xE9es \xE0 un fichier sur le syst\xE8me de fichiers. Les programmeurs\u2026"
title: "R\xE9diger un fichier texte"
weight: 24
---

## Quoi & Pourquoi ?
Écrire un fichier texte en Rust implique de créer, écrire et potentiellement ajouter des données à un fichier sur le système de fichiers. Les programmeurs effectuent cette opération pour persister des données, comme des journaux d'applications, des configurations ou du contenu généré par l'utilisateur, assurant la durabilité des données au-delà de la portée de l'exécution du programme.

## Comment faire :
La bibliothèque standard de Rust fournit des outils robustes pour la manipulation de fichiers, encapsulés principalement dans les modules `std::fs` et `std::io`. Voici un exemple de base pour créer et écrire dans un fichier texte :

```rust
use std::fs::File;
use std::io::prelude::*;

fn main() -> std::io::Result<()> {
    let mut file = File::create("bonjour.txt")?;
    file.write_all(b"Bonjour, monde !")?;
    Ok(())
}
```

Après avoir exécuté ce code, vous trouverez un fichier nommé `bonjour.txt` avec le contenu "Bonjour, monde !".

Pour des scénarios plus complexes, tels que l'ajout à un fichier ou la gestion de données plus volumineuses de manière efficace, Rust offre des fonctionnalités supplémentaires. Voici comment ajouter du texte à un fichier existant :

```rust
use std::fs::OpenOptions;
use std::io::prelude::*;

fn main() -> std::io::Result<()> {
    let mut file = OpenOptions::new()
        .write(true)
        .append(true)
        .open("bonjour.txt")?;
        
    file.write_all(b" Ajout de plus de texte.")?;
    Ok(())
}
```

L'exécution de ceci ajoutera " Ajout de plus de texte." à la fin de `bonjour.txt`.

Dans certains cas, l'utilisation de bibliothèques tierces peut simplifier les opérations sur les fichiers. La crate `serde`, combinée à `serde_json`, par exemple, permet de sérialiser et désérialiser des structures de données vers et à partir du format JSON, offrant une approche de haut niveau pour l'écriture de fichiers :

```rust
use serde::{Serialize, Deserialize};
use serde_json;
use std::fs::File;

#[derive(Serialize, Deserialize)]
struct Utilisateur {
    id: u32,
    nom: String,
}

fn main() -> std::io::Result<()> {
    let utilisateur = Utilisateur { id: 1, nom: "Jane Doe".into() };
    let fichier = File::create("utilisateur.json")?;
    serde_json::to_writer(fichier, &utilisateur)?;
    Ok(())
}
```

Après avoir exécuté le code ci-dessus, `utilisateur.json` contiendra une représentation JSON de la structure `Utilisateur`. Notez que l'utilisation de `serde` et `serde_json` nécessite d'ajouter ces crates à votre `Cargo.toml`.

Écrire des fichiers texte en Rust, que ce soit via la bibliothèque standard ou avec l'aide de crates externes, est une manière à la fois simple et puissante de gérer la persistance des données dans vos applications.
