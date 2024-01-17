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

## Quoi et Pourquoi?
Concaténer des chaînes de caractères est le fait de les mettre bout à bout afin d'en former une seule. Les programmeurs font cela pour combiner des textes et en créer un nouveau, par exemple pour afficher un message personnalisé.

## Comment faire:
Voici comment concatenar des chaînes en Rust:

```Rust
let prenom = "Jean";
let nom = "Dupont";
let message = format!("Bonjour {} {}, bienvenue!", prenom, nom);
println!("{}", message);
```

Résultat:

```
Bonjour Jean Dupont, bienvenue!
```

## Plongée en profondeur:
Concaténer des chaînes est une pratique courante en programmation, qui permet de créer des messages dynamiques à partir de variables. Cela peut également être fait en utilisant l'opérateur `+` ou en utilisant une macro spécifique `format!`. Il est à noter que cela peut entraîner une surcharge de mémoire si cela est fait à grande échelle.

## Voir aussi:
- [La documentation officielle de Rust](https://doc.rust-lang.org/book/ch08-02-strings.html)
- [Une comparaison entre les différentes façons de concatenar des chaînes en Rust](https://doc.rust-lang.org/std/string/struct.String.html#examples)
- [Un article sur les meilleures pratiques en matière de gestion de chaînes en Rust](https://medium.com/swlh/rust-best-practices-using-and-manipulating-strings-715baeaf7a11)