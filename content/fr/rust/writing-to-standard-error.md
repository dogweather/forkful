---
title:                "Écrire dans l'erreur standard"
html_title:           "Arduino: Écrire dans l'erreur standard"
simple_title:         "Écrire dans l'erreur standard"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/rust/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Quoi et Pourquoi ?
Écrire sur la sortie d'erreur standard (`stderr`) permet de séparer les messages d'erreur des résultats normaux (sortie standard, `stdout`). Les programmeurs font cela pour faciliter le débogage et permettre aux utilisateurs de rediriger les erreurs de manière appropriée.

## Comment faire :
```Rust
use std::io::{self, Write};

fn main() {
    writeln!(io::stderr(), "Ceci est une erreur!").expect("Échec de l'écriture sur stderr");
}
```
Sortie attendue dans `stderr` :
```
Ceci est une erreur!
```

## Exploration Approfondie
Historiquement, la différenciation entre `stdout` et `stderr` permet aux shells Unix de manipuler ces flux séparément. En Rust, on peut également utiliser la bibliothèque `log` pour une gestion plus sophistiquée des messages d'erreur. L'écriture vers `stderr` implique l'usage du module `std::io`, qui fournit les outils nécessaires pour une manipulation directe des entrées et sorties.

## Voir Aussi
- Documentation Rust sur `std::io`: https://doc.rust-lang.org/std/io/
- Le crate `log` pour la journalisation en Rust : https://crates.io/crates/log
- Guide pratique pour gérer stdout/stderr : https://rust-cli.github.io/book/in-depth/stdin-stdout-stderr.html
