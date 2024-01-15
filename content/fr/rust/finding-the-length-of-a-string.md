---
title:                "Trouver la longueur d'une chaîne."
html_title:           "Rust: Trouver la longueur d'une chaîne."
simple_title:         "Trouver la longueur d'une chaîne."
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/rust/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Pourquoi

Vous vous demandez peut-être pourquoi il est important de connaître la longueur d'une chaîne de caractères en programmation Rust. Eh bien, cela peut être extrêmement utile dans de nombreuses situations, que ce soit pour manipuler ou afficher des données de manière précise.

## Comment faire

Pour trouver la longueur d'une chaîne en Rust, vous pouvez utiliser la fonction `len()` de la structure `str`. Voici un exemple de code pour y parvenir:

```Rust
let string = "Bonjour!";
let length = string.len();

println!("La longueur de la chaîne est de {}", length);
```

Ce code créera une variable `length` qui stockera la longueur de la chaîne `string`. Ensuite, la fonction `println!` affichera la longueur à l'écran.

Voici un autre exemple qui affiche toutes les lettres d'une chaîne avec leur position respective:

```Rust
let string = "Hello";
let length = string.len();

for (i, c) in string.chars().enumerate() {
    println!("{} est en position {}", c, i);
}
```

La sortie de ce code sera:

```
H est en position 0
e est en position 1
l est en position 2
l est en position 3
o est en position 4
```

## Plongée en profondeur

Maintenant que vous savez comment trouver la longueur d'une chaîne en Rust, il est important de comprendre comment cela fonctionne en interne. En réalité, la fonction `len()` ne compte pas réellement les caractères d'une chaîne, mais plutôt les octets. Cela est dû au fait que Rust utilise l'encodage UTF-8 par défaut, où chaque caractère peut être codé sur plusieurs octets.

Cela signifie que la longueur d'une chaîne peut varier en fonction des caractères qu'elle contient. Par exemple, la chaîne "こんにちは" contient 5 caractères, mais sa longueur est de 15 octets car chaque caractère est codé sur 3 octets en UTF-8.

Il est également important de noter que la fonction `len()` ne compte que les caractères valides en UTF-8. Si une chaîne contient des octets invalides, la fonction renverra une longueur incorrecte.

## Voir aussi

- [Documentation officielle sur la structure `str`](https://doc.rust-lang.org/std/primitive.str.html)
- [Article sur l'encodage UTF-8 en Rust](https://towardsdatascience.com/understanding-utf-8-in-rust-a609705bb29b)