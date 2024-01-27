---
title:                "Rédaction de tests"
date:                  2024-01-19
html_title:           "Arduino: Rédaction de tests"
simple_title:         "Rédaction de tests"
programming_language: "Rust"
category:             "Rust"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/rust/writing-tests.md"
---

{{< edit_this_page >}}

## Quoi et Pourquoi ?

Les tests dans la programmation sont des vérifications automatisées pour s'assurer que notre code fonctionne correctement. On les écrit pour éviter les bugs et garantir que le code fait bien ce qu'on attend, même après des modifications.

## Comment faire :

Dans Rust, les tests unitaires se placent généralement dans le fichier qu'ils testent, dans un module nommé `tests` et annoté par `#[cfg(test)]`.

```Rust
// fonction à tester
fn saluer(nom: &str) -> String {
    format!("Bonjour, {}!", nom)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_saluer() {
        let resultat = saluer("Alice");
        assert_eq!(resultat, "Bonjour, Alice!");
    }
}
```

Pour lancer les tests, utilise:

```sh
cargo test
```

Sortie attendue:

```sh
running 1 test
test tests::test_saluer ... ok

test result: ok. 1 passed; 0 failed; 0 ignored; 0 measured; 0 filtered out
```

## Plongée Profonde

Historiquement, les tests sont un pilier du développement logiciel – ils remontent à l'époque du punch card. En Rust, `cargo test` est une commande intégrée qui exécute tests unitaires, d'intégration, et de documentation. Alternativement, on peut utiliser des frameworks externes comme `Proptest` ou `QuickCheck` pour des tests basés sur des propriétés. Rust exécute les tests en parallèle par défaut. Tu peux configurer les tests avec des annotations pour contrôler leur exécution, par exemple pour ignorer certains tests (`#[ignore]`) ou exécuter des codes avant et après les tests (`#[setup]` et `#[teardown]`).

## Voir Aussi

- [Le livre de Rust sur les tests](https://doc.rust-lang.org/book/ch11-00-testing.html)
- [Rust by Example sur les tests](https://doc.rust-lang.org/rust-by-example/testing.html)
- [Guide Cargo sur les commandes test](https://doc.rust-lang.org/cargo/guide/tests.html)
- [QuickCheck](https://docs.rs/quickcheck/latest/quickcheck/)
- [Proptest](https://docs.rs/proptest/0.10.1/proptest/)
