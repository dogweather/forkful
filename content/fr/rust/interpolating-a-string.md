---
title:                "Interpolation d'une chaîne de caractères"
html_title:           "Ruby: Interpolation d'une chaîne de caractères"
simple_title:         "Interpolation d'une chaîne de caractères"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/rust/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est et Pourquoi? (What & Why?)

1) L'interpolation de chaînes en programmation est le processus d'évaluation de variables contenues au sein d'une chaîne de caractères.
2) Les programmeurs l'utilisent pour rendre le code plus lisible et pour améliorer la concaténation des chaînes.

## Comment faire? (How to?)

```Rust
let nom = "Pierre";
let age = 30;
 
// On utilise `{}` pour interpoler
println!("Je m'appelle {} et j'ai {} ans.", nom, age);
```
*Sortie* :
```
Je m'appelle Pierre et j'ai 30 ans.
```
Utilisez `{}` pour l'interpolation dans `println!`.

## Plongeon Profond (Deep Dive)

1) **Contexte historique** : Rust, lancé en 2010, emprunte l'interpolation de chaînes à d'autres langages comme Ruby ou Python.
2) **Alternatives** : Plutôt que d'utiliser `println!`, on peut utiliser `format!` pour stocker le résultat dans une variable.
3) **Détails d'implémentation** : L'interpolation de chaînes en Rust n'est pas réalisée à l'exécution, mais à la compilation. C'est pourquoi elle est plus rapide et évite de nombreux bugs potentiels.

## Voir Aussi (See Also)

- [Documentation officielle Rust sur `println!`](https://doc.rust-lang.org/stable/std/macro.println.html)
- [Documentation officielle Rust sur `format!`](https://doc.rust-lang.org/std/fmt/index.html)
- [Discussion StackOverflow sur l'interpolation de chaînes](https://stackoverflow.com/questions/52494664/how-do-i-do-formatting-or-string-interpolation-in-rust)