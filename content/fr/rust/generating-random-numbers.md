---
title:                "Génération de nombres aléatoires"
html_title:           "Rust: Génération de nombres aléatoires"
simple_title:         "Génération de nombres aléatoires"
programming_language: "Rust"
category:             "Rust"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/rust/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Pourquoi utiliser des nombres aléatoires##

Utiliser des nombres aléatoires dans la programmation peut être utile pour de nombreux cas de figure. Que ce soit pour créer des jeux vidéos avec des éléments aléatoires ou pour tester l'efficacité d'un algorithme de chiffrement, les nombres aléatoires sont un outil précieux pour les développeurs. Cela permet également de rendre les programmes plus dynamiques et réalistes.

## Comment générer des nombres aléatoires en Rust##

Générer des nombres aléatoires en Rust est simple et facile grâce à la bibliothèque standard de Rust. Il suffit d'ajouter la ligne suivante au début de votre code :
```Rust
use rand::Rng;
```
Pour générer un nombre entier aléatoire dans une certaine plage, on peut utiliser la méthode `gen_range` :
```Rust
let random_num = rand::thread_rng().gen_range(1, 10);
```

Pour générer un nombre décimal aléatoire, on peut utiliser la méthode `gen_range` mais en spécifiant un type de données différent (comme `f32` ou `f64`) :
```Rust
let random_num = rand::thread_rng().gen_range(1.0, 10.0);
```

Il est également possible de générer un booléen aléatoire avec la méthode `gen_bool` :
```Rust
let random_bool = rand::thread_rng().gen_bool(0.5); // Renvoie true ou false avec une probabilité de 50%
```

## Plongée dans la génération de nombres aléatoires en Rust##

La bibliothèque standard de Rust utilise un algorithme de génération de nombres pseudo-aléatoires basé sur le générateur Mersenne Twister. Cela signifie que les nombres générés ne sont pas vraiment aléatoires mais ont l'apparence de l'être grâce à un calcul mathématique complexe.

Il est important de noter que le générateur de nombres aléatoires doit être initialisé avant de pouvoir utiliser les méthodes de la bibliothèque de génération de nombres aléatoires. Cela se fait en appelant la méthode `thread_rng()` qui renvoie un générateur de nombres initialisé spécifique au thread en cours.

## Voir aussi ##

- Documentation officielle de la bibliothèque `rand` : https://docs.rs/rand/0.7.3/rand/
- Tutoriel sur la génération de nombres aléatoires en Rust : https://stevedonovan.github.io/rust-gentle-intro/6-bonus.html
- Article sur la génération de nombres aléatoires en Rust : https://medium.com/@atshenoy/generating-random-numbers-in-rust-94070af294af