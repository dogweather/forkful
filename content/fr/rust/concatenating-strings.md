---
title:                "Concaténation de chaînes de caractères"
html_title:           "Rust: Concaténation de chaînes de caractères"
simple_title:         "Concaténation de chaînes de caractères"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/rust/concatenating-strings.md"
---

{{< edit_this_page >}}

## Pourquoi

La concaténation de chaînes de caractères est une tâche courante dans le développement de logiciel. Elle consiste à fusionner plusieurs chaînes de caractères en une seule. Cela peut être utile pour créer des messages personnalisés ou pour construire des requêtes de bases de données.

## Comment faire

Pour concaténer des chaînes de caractères en Rust, vous pouvez utiliser l'opérateur `+` ou la méthode `format!()`. Voici un exemple de code :

```Rust
let nom = "Jean";
let surnom = "Loulou";
let message = nom + " " + surnom; // Le résultat est "Jean Loulou"

let age = 25;
let phrase = format!("{} a {} ans", nom, age); // Le résultat est "Jean a 25 ans"
```

Comme vous pouvez le voir, l'opérateur `+` permet de concaténer des chaînes de caractères de manière plus simple, tandis que `format!()` offre plus de flexibilité en permettant d'ajouter des variables dynamiquement.

## Plongée en profondeur

Il est important de noter que l'opérateur `+` et la méthode `format!()` prennent en charge uniquement des types de données qui implémentent le trait `Display`. Cela signifie que certaines structures de données comme les vecteurs ou les tableaux ne peuvent pas être concaténés directement avec ces méthodes.

Cependant, Rust propose également la méthode `join()` qui peut être utilisée avec ces types de données pour les concaténer. Voici un exemple de code :

```Rust
let fruits = ["bananes", "pommes", "oranges"];
let liste = fruits.join(", "); // Le résultat est "bananes, pommes, oranges"
```

Il est également possible de concaténer des chaînes de caractères non constantes en utilisant la macro `concat!()`. Cette macro concatène les chaînes de caractères au moment de la compilation plutôt qu'à l'exécution, ce qui peut améliorer les performances.

Enfin, il est important de noter qu'en Rust, les chaînes de caractères sont immuables par défaut, ce qui signifie qu'elles ne peuvent pas être modifiées directement. Si vous avez besoin de modifier une chaîne de caractères, vous pouvez utiliser la méthode `to_owned()` pour la convertir en une chaîne de caractères mutable. Voici un exemple de code :

```Rust
let mut message = "Bonjour".to_owned();
message.push_str(" tout le monde!");
println!("{}", message); // Le résultat est "Bonjour tout le monde!"
```

## Voir aussi

- [Documentation Rust sur la concaténation de chaînes de caractères](https://doc.rust-lang.org/stable/std/string/struct.String.html#method.push_str)
- [Rust By Example: Strings](https://doc.rust-lang.org/stable/rust-by-example/std/str.html)
- [La documentation sur les macros Rust](https://doc.rust-lang.org/reference/macros.html)