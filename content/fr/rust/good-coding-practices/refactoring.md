---
title:                "Remaniement de code"
aliases:
- /fr/rust/refactoring.md
date:                  2024-01-26T03:36:47.775602-07:00
model:                 gpt-4-0125-preview
simple_title:         "Remaniement de code"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/rust/refactoring.md"
---

{{< edit_this_page >}}

## Quoi et Pourquoi ?

Le remaniement de code (ou refactoring) est le processus de restructuration du code informatique existant—changer la facturation—sans modifier son comportement externe. Les programmeurs le font pour améliorer les attributs non fonctionnels du logiciel, tels que la lisibilité, la réduction de la complexité, l'amélioration de la maintenabilité, et pour créer une architecture interne ou un modèle d'objet plus expressif afin d'améliorer l'extensibilité.

## Comment faire :

Refactorisons un simple morceau de code Rust pour le rendre plus idiomatique et maintenable. Nous commençons avec une fonction qui calcule la somme d'un vecteur d'entiers :

```rust
fn sum(vec: &Vec<i32>) -> i32 {
    let mut sum = 0;
    for i in vec {
        sum += i;
    }
    sum
}

fn main() {
    let numbers = vec![1, 2, 3, 4, 5];
    println!("La somme est {}", sum(&numbers));
}
```

Sortie :
```
La somme est 15
```

Maintenant, refactorisons cela pour utiliser un Rust plus idiomatique en exploitant les itérateurs et la méthode `fold` :

```rust
fn sum(vec: &[i32]) -> i32 {
    vec.iter().fold(0, |acc, &x| acc + x)
}

fn main() {
    let numbers = vec![1, 2, 3, 4, 5];
    println!("La somme est {}", sum(&numbers));
}
```

Pas de changement dans la sortie—c’est toujours `15`—mais la version refactorisée est plus propre et utilise les forces de Rust comme l'emprunt et les méthodes d'itérateur.

## Approfondissement

Le refactoring a ses racines dans la communauté Smalltalk et a été popularisé dans le monde Java par le livre de Martin Fowler "Refactoring: Improving the Design of Existing Code". Ses principes sont universels et s'appliquent également à Rust, où la sécurité et la concurrence sont primordiales. Rust encourage à écrire du code robuste en attrapant les problèmes au moment de la compilation, donc lors du refactoring, le compilateur Rust agit comme un filet de sécurité.

Les alternatives au refactoring manuel incluent l'utilisation d'outils automatisés, tels que 'rustfmt' pour le formatage de code et 'clippy' pour le linting, qui peuvent suggérer des manières plus idiomatiques d'écrire du code. Cependant, un refactoring approfondi nécessite souvent une compréhension réfléchie de la conception du code, que ces outils ne peuvent pas automatiser entièrement.

Dans Rust, le refactoring peut tourner autour de l'amélioration de l'utilisation des types, de l'exploitation efficace des durées de vie, de la réduction des allocations inutiles, ou de l'emploi de motifs de concurrence comme l'utilisation de `Arc<Mutex<T>>` lorsque nécessaire. Il est également courant de passer de `unwrap()` à une gestion d'erreur plus expressive avec `Result<T, E>`.

## Voir aussi

Pour approfondir le refactoring en Rust :

- Le Livre Rust : https://doc.rust-lang.org/book/
- Rust par l'Exemple : https://doc.rust-lang.org/rust-by-example/
- Clippy, un outil de linting Rust : https://github.com/rust-lang/rust-clippy
- "Refactoring: Improving the Design of Existing Code" par Martin Fowler : https://martinfowler.com/books/refactoring.html
