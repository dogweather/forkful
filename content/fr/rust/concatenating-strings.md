---
title:                "Rust: Concaténation de chaînes"
simple_title:         "Concaténation de chaînes"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/rust/concatenating-strings.md"
---

{{< edit_this_page >}}

## Pourquoi

La concaténation de chaînes de caractères est une tâche courante en programmation, et c'est encore plus vrai en Rust où les chaînes de caractères sont des types distincts. Elle est utile pour joindre plusieurs chaînes ensemble afin de créer une chaîne plus longue qui contient toutes les informations nécessaires. Dans cet article, nous allons explorer comment réaliser cette tâche en Rust.

## Comment faire

Il existe plusieurs façons de concaténer des chaînes en Rust, vous pouvez utiliser l'opérateur `+`, la macro `format!` ou encore la méthode `push_str`. Voyons quelques exemples concrets :

```Rust
let name = "John";
let message = "Hello " + name + "!"; // Utilisation de l'opérateur +
println!("{}", message); // Affiche "Hello John!"
```

```Rust
let num1 = 10;
let num2 = 20;
let result = format!("{} + {} = {}", num1, num2, num1 + num2); // Utilisation de la macro format!
println!("{}", result); // Affiche "10 + 20 = 30"
```

```Rust
let mut str1 = String::from("Bonjour ");
let str2 = "le monde";
str1.push_str(str2); // Utilisation de la méthode push_str
println!("{}", str1); // Affiche "Bonjour le monde"
```

Comme vous pouvez le constater, toutes ces méthodes permettent de réaliser la concaténation de chaînes en Rust, à vous de choisir celle qui correspond le mieux à votre besoin.

## Plongée en profondeur

Il est important de noter que certaines de ces méthodes prennent ownership des chaînes, tandis que d'autres les empruntent. Cela peut avoir un impact sur la gestion de la mémoire et la performance de votre code. De plus, lors de la concaténation de plusieurs chaînes, il est recommandé d'utiliser la macro `format!` plutôt que l'opérateur `+` car cela évite de créer de nouvelles chaînes à chaque concaténation.

Vous pouvez également utiliser des slices pour concaténer des chaînes sans avoir à effectuer de transfert de propriété. Par exemple :

```Rust
let str1 = String::from("Hello");
let str2 = " world";
let str3 = str1 + str2; // Erreur de compilation car str1 est ownership
let str3 = str1[..] + str2; // Utilisation de slices pour résoudre le problème
```

## Voir aussi

Pour en savoir plus sur la concaténation de chaînes en Rust, vous pouvez consulter les liens suivants :

- [La documentation officielle de Rust sur les chaînes de caractères](https://doc.rust-lang.org/std/string/index.html)
- [Un article sur les meilleures pratiques de gestion de mémoire en Rust](https://medium.com/dwelo-r-d/dos-and-donts-of-owning-strings-in-rust-23fdfdfa3fdf)
- [Un tutoriel interactif pour apprendre Rust en pratiquant](https://www.rust-lang.org/learn)