---
title:                "Rust: Ecrire des tests"
programming_language: "Rust"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/rust/writing-tests.md"
---

{{< edit_this_page >}}

## Pourquoi

Écrire des tests est une pratique importante dans le monde de la programmation, et cela ne fait pas exception en Rust. Les tests permettent de garantir la qualité du code et de détecter les erreurs avant qu'elles n'affectent l'application en production.

## Comment faire

Pour écrire des tests en Rust, il suffit d'utiliser l'attribut `#[test]` au-dessus de la fonction de test. Par exemple:

````Rust
#[test]
fn test_addition() {
    assert_eq!(2 + 2, 4);
}
````

Dans cet exemple, nous testons une fonction d'addition basique en utilisant l'assertion `assert_eq!`, qui vérifie si le résultat attendu est bien égal au résultat réel. Si ce n'est pas le cas, le test échouera.

Il est également possible d'utiliser des blocs `assert!` pour tester des conditions plus complexes. Par exemple:

````Rust
#[test]
fn test_max() {
    let numbers = vec![1, 5, 3, 2, 4];
    assert_eq!(numbers.iter().max(), Some(&5));
}
````

Dans cet exemple, nous testons si le résultat attendu lors du calcul du maximum d'un vecteur de nombres correspond bien au résultat réel.

## Zoom sur les tests

En plus des fonctions de test, il est possible de créer des tests unitaires pour chaque fonction ou méthode dans votre code. Cela peut être fait en utilisant l'attribut `#[cfg(test)]` au-dessus de votre code et en plaçant les tests dans le même fichier que les fonctions à tester.

De plus, Rust offre la possibilité d'utiliser des macros de tests personnalisées pour effectuer des tâches spécifiques avant et après chaque test, ainsi que pour regrouper plusieurs tests en une seule fonction.

## Voir aussi

- Documentation officielle: https://doc.rust-lang.org/book/ch11-01-writing-tests.html
- Guide sur les tests en Rust: https://blog.mgattozzi.dev/writing-tests-in-rust
- Vidéo sur les tests en Rust: https://www.youtube.com/watch?v=EfmeWgOob4Y