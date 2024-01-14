---
title:                "Rust: Trouver la longueur d'une chaîne de caractères."
simple_title:         "Trouver la longueur d'une chaîne de caractères."
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/rust/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Pourquoi

Vous pourriez vous demander, pourquoi est-ce important de connaître la longueur d'une chaîne de caractères en Rust ? Eh bien, cela peut être utile lors de la manipulation de données ou de l'affichage de chaînes de caractères dans une certaine limite de longueur.

## Comment faire

Pour trouver la longueur d'une chaîne de caractères en Rust, vous pouvez utiliser la méthode `len()` qui renvoie un entier représentant le nombre de caractères dans la chaîne.

Voici un exemple de code pour trouver la longueur d'une chaîne de caractères :

```rust
let s = String::from("Bonjour !");
let len = s.len();

println!("La longueur de la chaîne est : {}", len); // output: La longueur de la chaîne est : 9
```

Dans cet exemple, nous créons une nouvelle chaîne de caractères `s` et utilisons la méthode `len()` pour trouver sa longueur. Nous imprimons ensuite la longueur à l'aide de la fonction `println!()`.

## Plongée en profondeur

Vous vous demandez peut-être comment la méthode `len()` fonctionne en interne. En fait, elle compte le nombre d'octets dans la chaîne de caractères et ne tient pas compte des caractères Unicode. Cela signifie qu'une chaîne de caractères contenant des caractères non ASCII aura une longueur différente de celle attendue.

Pour obtenir la longueur réelle de la chaîne de caractères en prenant en compte les caractères Unicode, vous pouvez utiliser la méthode `chars()` qui sépare chaque caractère de la chaîne et ensuite utiliser la méthode `count()` pour compter le nombre de caractères.

Voici un exemple de code pour trouver la longueur d'une chaîne de caractères en utilisant `count()`:

```rust
let s = String::from("Bonjour !");
let len = s.chars().count();

println!("La longueur de la chaîne est : {}", len); // output: La longueur de la chaîne est : 8
```

Comme vous pouvez le voir, la longueur est différente de l'exemple précédent car cette fois-ci, nous prenons en compte les caractères Unicode.

## Voir aussi

- [Documentation de la méthode `len()` en Rust](https://doc.rust-lang.org/std/string/struct.String.html#method.len)
- [Documentation de la méthode `chars()` en Rust](https://doc.rust-lang.org/std/string/struct.String.html#method.chars)
- [Documentation de la méthode `count()` en Rust](https://doc.rust-lang.org/std/str/struct.Chars.html#method.count)