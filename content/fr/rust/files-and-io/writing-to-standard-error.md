---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:34:30.824451-07:00
description: "\xC9crire sur l'erreur standard (stderr) en Rust consiste \xE0 diriger\
  \ les messages d'erreur et les diagnostics vers la console s\xE9par\xE9ment de la\
  \ sortie standard\u2026"
lastmod: '2024-03-13T22:44:57.507908-06:00'
model: gpt-4-0125-preview
summary: "\xC9crire sur l'erreur standard (stderr) en Rust consiste \xE0 diriger les\
  \ messages d'erreur et les diagnostics vers la console s\xE9par\xE9ment de la sortie\
  \ standard\u2026"
title: "\xC9crire sur l'erreur standard"
weight: 25
---

## Quoi et pourquoi ?
Écrire sur l'erreur standard (stderr) en Rust consiste à diriger les messages d'erreur et les diagnostics vers la console séparément de la sortie standard (stdout). Les programmeurs font cela pour différencier la sortie normale du programme des messages d'erreur, ce qui facilite la gestion appropriée des erreurs ou leur redirection vers des journaux ou des fichiers pendant l'exécution.

## Comment faire :
Rust propose une manière simple d'écrire sur stderr en utilisant la macro `eprintln!`, similaire à l'utilisation de `println!` pour stdout. Voici un exemple basique :

```rust
fn main() {
    eprintln!("Ceci est un message d'erreur !");
}
```

Exemple de sortie (vers l'erreur standard) :
```
Ceci est un message d'erreur !
```

Pour plus de contrôle sur les messages d'erreur, comme lorsque vous souhaitez formater du texte ou gérer les résultats d'E/S, utilisez la fonction `stderr` du module `std::io`. Cette méthode fournit un accès au flux stderr global, sur lequel vous pouvez ensuite écrire en utilisant des méthodes telles que `write_all` ou `writeln` de la caractéristique `Write` :

```rust
use std::io::{self, Write};

fn main() {
    let stderr = io::stderr();
    let mut handle = stderr.lock();
    
    writeln!(handle, "Message d'erreur formaté : {}", 404).expect("Échec de l'écriture sur stderr");
}
```

Exemple de sortie (vers l'erreur standard) :
```
Message d'erreur formaté : 404
```

Si vous travaillez dans des environnements ou des applications où vous vous reposez sur des bibliothèques pour la journalisation ou la gestion des erreurs, des bibliothèques telles que `log` et `env_logger` sont populaires. Bien qu'elles soient utilisées davantage à des fins de journalisation, elles sont configurables et peuvent diriger les niveaux de journalisation d'erreurs vers stderr. Ci-dessous, un exemple d'utilisation simple avec `log` et `env_logger` :

D'abord, ajoutez les dépendances à votre `Cargo.toml` :
```toml
[dependencies]
log = "0.4"
env_logger = "0.9"
```

Ensuite, configurez et utilisez la journalisation dans votre application :
```rust
fn main() {
    env_logger::init();
    log::error!("Ceci est un message d'erreur journalisé sur stderr");
}
```

Exécuter ce programme (après avoir configuré `env_logger` avec une variable d'environnement appropriée, par exemple, `RUST_LOG=error`) sortira le message d'erreur sur stderr, en utilisant l'infrastructure de journalisation.

```plaintext
ERROR: Ceci est un message d'erreur journalisé sur stderr
```
