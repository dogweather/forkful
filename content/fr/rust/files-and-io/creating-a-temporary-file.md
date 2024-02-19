---
aliases:
- /fr/rust/creating-a-temporary-file/
date: 2024-01-20 17:41:18.659281-07:00
description: "Cr\xE9er un fichier temporaire, c'est une fa\xE7on d'avoir un stockage\
  \ \xE9ph\xE9m\xE8re pendant l'ex\xE9cution d'un programme. Les programmeurs le font\
  \ pour manipuler des\u2026"
lastmod: 2024-02-18 23:09:08.550740
model: gpt-4-1106-preview
summary: "Cr\xE9er un fichier temporaire, c'est une fa\xE7on d'avoir un stockage \xE9\
  ph\xE9m\xE8re pendant l'ex\xE9cution d'un programme. Les programmeurs le font pour\
  \ manipuler des\u2026"
title: "Cr\xE9ation d'un fichier temporaire"
---

{{< edit_this_page >}}

## What & Why?
Créer un fichier temporaire, c'est une façon d'avoir un stockage éphémère pendant l'exécution d'un programme. Les programmeurs le font pour manipuler des données sans affecter le système de fichiers permanent.

## How to:
Rust a le crate `tempfile` pour simplifier la création de fichiers temporaires. Voici comment l'utiliser :

```rust
// Importez le crate `tempfile`
use tempfile::NamedTempFile;
use std::io::{Write, Read};

fn main() -> std::io::Result<()> {
    // Créer un fichier temporaire
    let mut fichier_temp = NamedTempFile::new()?;

    // Écrivez quelque chose dedans
    fichier_temp.write_all(b"Bonjour le monde temporaire!")?;

    // Lisez le contenu
    let mut contenu = String::new();
    fichier_temp.seek(std::io::SeekFrom::Start(0))?;
    fichier_temp.read_to_string(&mut contenu)?;

    println!("Contenu du fichier: {}", contenu);
    
    // Le fichier est supprimé ici
    drop(fichier_temp);
    
    Ok(())
}
```

## Deep Dive
Historiquement, les fichiers temporaires servaient à stocker des données excédant la mémoire, ou pour sécuriser des transactions (rollback possible). Aujourd'hui, avec 'tempfile' en Rust, il s'agit surtout de garantir un nom de fichier unique et d'éviter des conflits entre fils d'exécution (thread-safety). Les fichiers se suppriment automatiquement, réduisant les risques de laisser des données sensibles. Par rapport aux fichiers traditionnels, 'tempfile' gère tout cela proprement, sans que vous vous cassiez la tête.

Alternativement, `std::fs` peut être utilisé pour plus de contrôle, au prix d'une complexité accrue. Mais `tempfile` reste le moyen le plus sûr et le plus simple pour un travail rapide et sûr.

## See Also
Pour plus d'informations, consultez la documentation et les sources suivantes :
- [Documentation officielle de tempfile](https://docs.rs/tempfile/)
- [Rust by Example pour la manipulation de fichiers](https://doc.rust-lang.org/rust-by-example/std_misc/file.html)
