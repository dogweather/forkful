---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:32:00.402061-07:00
description: "Comment faire : Le framework de test int\xE9gr\xE9 \xE0 Rust prend en\
  \ charge les tests unitaires, d'int\xE9gration et de documentation sans n\xE9cessiter\
  \ de\u2026"
lastmod: '2024-03-13T22:44:57.485592-06:00'
model: gpt-4-0125-preview
summary: "Le framework de test int\xE9gr\xE9 \xE0 Rust prend en charge les tests unitaires,\
  \ d'int\xE9gration et de documentation sans n\xE9cessiter de biblioth\xE8ques externes."
title: "R\xE9daction de tests"
weight: 36
---

## Comment faire :
Le framework de test intégré à Rust prend en charge les tests unitaires, d'intégration et de documentation sans nécessiter de bibliothèques externes. Les tests sont annotés avec `#[test]`, et toute fonction ainsi annotée est compilée comme un test.

### Écrire un Test Unitaire :
Placez les tests unitaires dans le module qu'ils testent en utilisant un sous-module `tests` marqué avec `#[cfg(test)]` pour garantir qu'ils sont uniquement compilés lors du test.

```rust
// lib.rs ou main.rs
pub fn add(a: i32, b: i32) -> i32 {
    a + b
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_adds_two() {
        assert_eq!(add(2, 2), 4);
    }
}
```

Exécuter les tests :
```shell
$ cargo test
```

Sortie :
```shell
   Compilation de your_package_name v0.1.0 (/chemin/vers/votre_paquet)
    Fin de la compilation test [non optimisé + debuginfo] cible(s) en 0.00 secs
     Exécution des unittests src/lib.rs (ou src/main.rs)

exécution de 1 test
test tests::it_adds_two ... ok

résultat du test : ok. 1 passé ; 0 échoué ; 0 ignoré ; 0 mesuré ; 0 filtré
```

### Écrire des Tests d'Intégration :
Les tests d'intégration se placent dans un répertoire tests au niveau supérieur de votre projet, à côté de `src`. Chaque fichier `.rs` dans `tests` est compilé comme sa propre crate séparée.

```rust
// tests/integration_test.rs
use your_package_name;

#[test]
fn it_adds_two() {
    assert_eq!(your_package_name::add(2, 2), 4);
}
```

### Tester avec des Bibliothèques Tierces Populaires :
Pour des capacités de test plus étendues, la bibliothèque `proptest` peut générer une large gamme d'entrées pour tester les fonctions.

Ajoutez `proptest` comme une dépendance de développement dans `Cargo.toml` :

```toml
[dev-dependencies]
proptest = "1.0"
```

Utilisez `proptest` pour exécuter le même test avec de nombreuses entrées générées automatiquement :

```rust
// à l'intérieur de tests/integration_test.rs ou un module's #[cfg(test)]

use proptest::prelude::*;

proptest! {
    #[test]
    fn doesnt_crash(a: i32, b:i32) {
        your_package_name::add(a, b);
    }
}
```

Cela vérifie que `add` ne panique pas pour une large gamme d'entrées `i32`.
