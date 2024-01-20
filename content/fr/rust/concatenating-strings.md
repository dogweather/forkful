---
title:                "Concaténation de chaînes"
html_title:           "C: Concaténation de chaînes"
simple_title:         "Concaténation de chaînes"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/rust/concatenating-strings.md"
---

{{< edit_this_page >}}

# Jouons avec les chaines de caractères en Rust!

## Quoi & Pourquoi?
La concaténation de chaînes est l'opération de coller ensemble deux morceaux de texte. Les programmeurs le font pour construire des messages d'erreur, des logs, des écrans UI et d'autres sorties basées sur le texte.

## Comment faire:
Voici comment nous pouvons concaténer des chaînes en Rust:

```Rust
fn main() {
    let salut = String::from("Salut, ");
    let world = String::from("monde");
    let message = salut + &world; // Notez l'usage du &

    println!("{}", message);
}
```

La sortie sera:

```Rust
Salut, monde
```

## Divons un peu plus profondément
En Rust, concaténer des chaînes a des subtilités. Vous utilisez un "&" à cause du système d'emprunt de Rust. Dans d'autres langages, une nouvelle chaîne est simplement créée. Mais Rust est plus consciencieux en matière de gestion de la mémoire.

Il y a d'autres façons de concaténer des chaînes. Vous pouvez utiliser la méthode `format!`:

```Rust
let message = format!("{}{}", salut, world);
```

Cependant, l'opérateur `+` est généralement le plus efficace en termes de performances.

## Pour en savoir plus
- La documentation officielle de Rust offre des explications détaillées sur la [gestion des chaînes](https://doc.rust-lang.org/book/ch08-02-strings.html)
- Cela vaut la peine de jeter un œil à ce thread de [StackOverflow](https://stackoverflow.com/questions/30154541) discutant de la performance de différentes méthodes de concaténation de chaînes.
- Voici un [billet de blog](https://fasterthanli.me/articles/small-strings-in-rust) intéressant sur l'implémentation des chaînes en Rust.