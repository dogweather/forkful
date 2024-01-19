---
title:                "Vérifier si un répertoire existe"
html_title:           "Lua: Vérifier si un répertoire existe"
simple_title:         "Vérifier si un répertoire existe"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/rust/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi?

Vérifier si un répertoire existe est une opération courante qui consiste à déterminer si un chemin particulier mène à un répertoire existant ou non. Les programmeurs le font pour éviter les erreurs inattendues lors de l'interaction avec des fichiers et des répertoires.

## Comment faire:

Pour vérifier si un répertoire existe en Rust, on utilise les méthodes `metadata()` ou `exists()` de la classe `std::fs::Path`. Voici un exemple:

```Rust
use std::path::Path;

fn main() {
    let path = Path::new("/chemin/vers/le/repertoire");
    let existe = path.metadata().is_ok();

    println!("Le repertoire existe: {}", existe);
}
```

Et voici ce que vous verrez comme sortie:

```
Le repertoire existe: true
```

Si le chemin mène à un répertoire existant, la valeur retournée sera "true". Sinon, elle sera "false".

## Détails techniques

Historiquement, Rust utilise des classes de la bibliothèque standard `std::fs::Path` pour manipuler les chemins de fichiers. L'approche présentée ici - utiliser `metadata()` ou `exists()` - est un choix simple et efficace pour vérifier l'existence d'un répertoire.

Les alternatives peuvent être plus complexes et moins efficaces. Vous pourriez par exemple tenter d'ouvrir le répertoire avec `std::fs::read_dir()` et vérifier si une erreur se produit. Cependant, utiliser `metadata()` ou `exists()` a l'avantage d'être plus rapide et direct.

En effet, ces deux méthodes vérifient simplement si le système de fichiers peut obtenir des métadonnées pour le chemin donné. Aucun fichier ou répertoire n'est ouvert. Par conséquent, il n'y a pratiquement aucune performance à perdre.

## Voir aussi:

Pour plus d'informations sur la manipulation de fichiers et de répertoires en Rust, consultez les ressources suivantes:

- Documentation officielle de Rust sur std::fs: https://doc.rust-lang.org/std/fs/index.html
- Un excellent tutoriel sur la manipulation des fichiers en Rust par le site tuto.com : https://www.tuto.com/rust/manipulation-fichiers-rust,2345.html