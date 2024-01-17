---
title:                "Interpoler une chaîne de caractères"
html_title:           "Rust: Interpoler une chaîne de caractères"
simple_title:         "Interpoler une chaîne de caractères"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/rust/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi?

Interpoler une chaîne de caractères, c'est simplement insérer dynamiquement une valeur ou une expression dans une chaîne de caractères. Les programmeurs le font pour rendre leurs chaînes de caractères plus dynamiques et flexibles, en leur permettant d'intégrer des valeurs générées à partir de variables ou d'expressions.

## Comment faire:

```Rust
let name = "John";
println!("Salut, je m'appelle {}!", name);

// Output: Salut, je m'appelle John!
```

Dans cet exemple, nous avons déclaré une variable `name` avec la valeur "John" et nous l'avons ensuite interpolée dans notre chaîne de caractères lors de l'utilisation de la fonction `println!()`.

```Rust
let age = 25;
println!("J'ai {} ans.", age);

// Output: J'ai 25 ans.
```

Dans cet exemple, nous avons interpolé une expression dans notre chaîne de caractères en utilisant la variable `age` pour afficher notre âge.

## Plongée en profondeur:

Interpoler des chaînes de caractères n'est pas une fonctionnalité spécifique à Rust, on la retrouve également dans d'autres langages de programmation tels que Python et JavaScript. Cependant, Rust a implémenté cette fonctionnalité de manière efficace en utilisant les macros, qui sont des structures de données et des traitements similaires à ceux des fonctions.

## Voir aussi:

- [Documentation officielle de Rust sur les chaînes de caractères](https://doc.rust-lang.org/std/string/)
- [Tutoriel sur les chaînes de caractères en Rust](https://www.rust-lang.org/learn/get-started)
- [Guide de programmation Rust sur les macros](https://doc.rust-lang.org/1.30.0/book/first-edition/macros.html)