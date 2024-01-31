---
title:                "Travailler avec TOML"
date:                  2024-01-26T04:26:11.605893-07:00
model:                 gpt-4-0125-preview
simple_title:         "Travailler avec TOML"

category:             "Rust"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/rust/working-with-toml.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?
TOML est un langage de sérialisation de données lisible par l'homme, souvent utilisé pour les configurations. Les programmeurs utilisent TOML pour sa simplicité et sa clarté, se traduisant facilement en une table de hachage en Rust.

## Comment faire :
```Rust
// 1. Inclure la crate 'toml' dans votre Cargo.toml
// [dépendances]
// toml = "0.5"

// 2. Désérialiser TOML dans une structure en Rust
use toml::Value;

fn main() {
    let contenu_toml = r#"
        [serveur]
        hôte = "localhost"
        port = 8080
    "#;

    let valeur = contenu_toml.parse::<Value>().unwrap();
    let hôte = valeur.get("serveur").unwrap().get("hôte").unwrap();
    let port = valeur.get("serveur").unwrap().get("port").unwrap();
    
    println!("Le serveur fonctionne sur {}:{}", hôte, port);
    // Sortie : Le serveur fonctionne sur "localhost":8080
}
```

## Approfondissement
TOML, qui signifie "Tom's Obvious, Minimal Language" (Le Langage Évident et Minimal de Tom), a été créé par Tom Preston-Werner en 2013. Il vise à être plus lisible que JSON ou YAML pour les fichiers de configuration. La conception de TOML se concentre sur une syntaxe sans ambiguïté, le minimalisme et une cartographie facile vers des types de données.

Les alternatives à TOML incluent JSON, YAML et XML, mais TOML l'emporte dans des scénarios où la lisibilité humaine et l'édition de fichiers par des non-programmeurs sont cruciales. Lors du travail avec TOML en Rust, serde fournit une base solide pour la sérialisation et la désérialisation, en utilisant des traits pour mapper TOML sur les structures de Rust sans effort.

Un défi lors du travail avec TOML est sa strictesse sur les types et la structure. Le programmeur doit définir un système de types Rust bien structuré reflétant le schéma des données TOML pour utiliser efficacement TOML en Rust.

## Voir Aussi
- [Documentation TOML](https://toml.io/en/)
- [Crate serde_toml](https://docs.rs/serde_toml/)
- [Livre sur le Langage de Programmation Rust](https://doc.rust-lang.org/stable/book/)
- [Dépôt GitHub TOML](https://github.com/toml-lang/toml)
