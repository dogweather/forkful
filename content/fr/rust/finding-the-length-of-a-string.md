---
title:                "Trouver la longueur d'une chaîne de caractères"
html_title:           "Rust: Trouver la longueur d'une chaîne de caractères"
simple_title:         "Trouver la longueur d'une chaîne de caractères"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/rust/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi?
La longueur d'une chaîne de caractères fait référence au nombre de caractères présents dans cette chaîne. Les programmeurs ont souvent besoin de trouver la longueur d'une chaîne pour des tâches telles que la manipulation de données, la validation de saisie ou la création de boucles. 

## Comment faire:
Voici un exemple de code en Rust pour trouver la longueur d'une chaîne de caractères et l'afficher en sortie:

```Rust
let my_string = "Hello World!";
let string_length = my_string.len();
println!("Longueur de la chaîne: {}", string_length);
```

Output: 
```
Longueur de la chaîne: 12
```

Vous pouvez également utiliser la méthode `.len()` sur une chaîne de caractères sans la stocker dans une variable:

```Rust
let my_string = "Hola";
println!("Longueur de la chaîne: {}", my_string.len());
```

Output: 
```
Longueur de la chaîne: 4
```

## Plongée en profondeur:
Trouver la longueur d'une chaîne de caractères peut sembler simple, mais cela n'a pas toujours été le cas. Dans les anciennes versions de Rust, la méthode pour trouver la longueur d'une chaîne était différente. Aujourd'hui, il existe plusieurs alternatives pour trouver la longueur d'une chaîne de caractères, telles que l'utilisation de la méthode `.chars()` ou le comptage du nombre de caractères avec une boucle.

## Voir aussi:
- [Documentation officielle Rust sur les chaînes](https://doc.rust-lang.org/std/string/struct.String.html#method.len)
- [Exemple de code pour trouver la longueur d'une chaîne en C++](https://www.programiz.com/cpp-programming/library-function/cstring/strlen)
- [Article sur les différentes méthodes pour trouver la longueur d'une chaîne en Python](https://www.geeksforgeeks.org/python-string-len-method/)