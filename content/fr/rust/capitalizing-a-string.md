---
title:                "Rust: Majuscule d'une chaîne de caractères"
programming_language: "Rust"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/rust/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Pourquoi

Dans la programmation, il est souvent nécessaire de manipuler du texte, et une des tâches communes est de capitaliser une chaîne de caractères. Cela peut sembler simple, mais il peut y avoir des nuances et des cas d'utilisation spécifiques qui nécessitent une approche plus réfléchie. C'est pourquoi nous allons plonger dans le monde passionnant de la capitalisation de chaînes en Rust.

## Comment faire

Pour capitaliser une chaîne de caractères en Rust, nous pouvons utiliser la méthode `to_uppercase()` de la structure `String`. Elle prend en paramètre une référence mutable vers une chaîne de caractères et renvoie une nouvelle chaîne de caractères avec toutes les lettres en majuscules. Voyons un exemple concret :

```Rust
let mut my_string = String::from("bonjour tout le monde");
let capitalized = my_string.to_uppercase();
println!("{}", capitalized); // affiche "BONJOUR TOUT LE MONDE"
```

Vous pouvez également capitaliser uniquement la première lettre d'une chaîne en utilisant la méthode `capitalize()` de la structure `String`. Elle prend également une référence mutable vers une chaîne de caractères et renvoie une nouvelle chaîne de caractères avec la première lettre en majuscule. Voici un exemple :

```Rust
let mut my_string = String::from("bonjour");
let capitalized = my_string.capitalize();
println!("{}", capitalized); // affiche "Bonjour"
```

## Plongée en profondeur

Bien que la capitalisation puisse sembler simple, il y a des détails à prendre en compte lors de l'utilisation de ces méthodes en Rust. Par exemple, il est important de noter que la méthode `to_uppercase()` ne fonctionne que pour les chaînes de caractères ASCII. Si vous travaillez avec des chaînes de caractères Unicode, vous devrez utiliser la bibliothèque `unicode_normalization` pour un traitement plus complet.

De plus, ces méthodes ne modifient pas la chaîne de caractères d'origine, mais renvoient plutôt une nouvelle chaîne de caractères capitalisée. Cela peut être utile si vous avez besoin de garder la chaîne de caractères d'origine pour une utilisation ultérieure.

## Voir aussi

- [La documention sur les méthodes de manipulation de chaînes en Rust](https://doc.rust-lang.org/std/string/struct.String.html)
- [La bibliothèque `unicode_normalization` pour une manipulation plus avancée des chaînes Unicode](https://crates.io/crates/unicode-normalization)
- [Un tutoriel sur les chaînes en Rust](https://doc.rust-lang.org/book/ch08-02-strings.html)