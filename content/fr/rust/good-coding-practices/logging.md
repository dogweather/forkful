---
title:                "Journalisation"
aliases:
- /fr/rust/logging/
date:                  2024-01-26T01:08:40.580902-07:00
model:                 gpt-4-1106-preview
simple_title:         "Journalisation"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/rust/logging.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?

La journalisation est comme tenir un journal pour votre application ; c'est la pratique d'enregistrer des événements, des erreurs et d'autres données pertinentes pendant l'exécution. Les développeurs utilisent les journaux pour diagnostiquer des problèmes, surveiller le comportement du système et recueillir des informations qui entraînent des améliorations – c'est le pain et le beurre de l'intelligence opérationnelle.

## Comment faire :

Mettre en place un scénario de journalisation de base en Rust en utilisant la crate `log`, qui fournit une façade de journalisation, et `env_logger`, une implémentation de journalisation pour la crate `log`. Tout d'abord, ajoutez-les à votre Cargo.toml :

```toml
[dependencies]
log = "0.4.14"
env_logger = "0.9.0"
```

Maintenant, configurez et initialisez le journaliseur dans votre `main.rs` :

```rust
use log::{info, warn};

fn main() {
    env_logger::init();

    info!("Ceci est un message d'info.");
    warn!("Ceci est un message d'avertissement.");
}
```

Exécutez votre application avec `RUST_LOG=info cargo run`, et vous verrez la sortie :

```
INFO: Ceci est un message d'info.
WARN: Ceci est un message d'avertissement.
```

Expérimentez avec la variable d'environnement `RUST_LOG` en la réglant sur `error`, `warn`, `info`, `debug`, ou `trace` pour contrôler la verbosité de vos journaux.

## Plongée Profonde

Le concept de journalisation n'est pas nouveau ; il existe depuis les premiers jours de l'informatique. Avant que la journalisation ne soit commune dans les logiciels, les développeurs comptaient sur des méthodes primitives telles que les déclarations d'impression ou les outils de débogage pour tracer l'exécution des programmes. À mesure que les programmes devenaient plus complexes, le besoin d'approches structurées à la journalisation augmentait également.

En Rust, la crate `log` abstrait les détails d'implémentation de la journalisation, permettant aux développeurs de brancher différents backends de journalisation. Bien que `env_logger` soit un choix courant, il existe des alternatives comme `fern`, `slog` ou `tracing`, chacune avec son propre ensemble de fonctionnalités et d'options de configuration.

Quelques considérations lors de la mise en œuvre de la journalisation comprennent :

1. **Niveaux de Log** : Contrôler la verbosité est clé. La crate `log` de Rust définit plusieurs niveaux de log : error, warn, info, debug et trace, dans l'ordre décroissant de gravité.

2. **Performance** : La journalisation peut avoir un impact sur la performance. Il est crucial de l'utiliser avec discernement, en veillant à éviter la journalisation dans les chemins critiques pour la performance ou les journaux excessivement verbeux en production.

3. **Journalisation Structurée** : Les meilleures pratiques modernes impliquent une journalisation structurée, où les journaux sont écrits dans un format lisible par machine comme JSON. Des bibliothèques comme `slog` permettent une journalisation structurée en Rust, qui peut être indexée et interrogée en utilisant des systèmes de gestion de journaux comme ELK Stack ou Splunk.

4. **Journalisation Asynchrone** : Pour minimiser l'impact sur l'application principale, la journalisation peut être effectuée de manière asynchrone. Cela est souvent réalisé en faisant en sorte que la bibliothèque de journalisation écrive dans une file d'attente en mémoire, et un fil séparé traite la file et écrit les journaux vers la destination.

5. **Configuration** : De nombreux cadres de journalisation prennent en charge la configuration par le biais de variables d'environnement, de fichiers de configuration et/ou de code. Cette flexibilité est clé pour peaufiner la sortie dans différents environnements (développement, mise en scène, production).

## Voir Aussi

- La documentation de la crate `log` : https://docs.rs/log/
- La documentation de la crate `env_logger` : https://docs.rs/env_logger/
- La page de journalisation de Rust by Example : https://doc.rust-lang.org/rust-by-example/std_misc/log.html
- La crate `slog`, un cadre de journalisation alternatif : https://github.com/slog-rs/slog
- Tracing, un cadre pour instrumenter les programmes Rust : https://crates.io/crates/tracing
