---
title:                "Majuscule d'une chaîne de caractères"
html_title:           "Rust: Majuscule d'une chaîne de caractères"
simple_title:         "Majuscule d'une chaîne de caractères"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/rust/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi?

La capitalisation d'une chaîne de caractères, c'est lorsqu'on met en majuscule la première lettre de chaque mot et en minuscule le reste des lettres. Les programmeurs le font souvent pour des raisons d'esthétique et de lisibilité, ainsi que pour suivre des conventions de codage.

## Comment faire:

Voici un exemple de code en Rust pour capitaliser une chaîne de caractères :

```Rust
let ma_chaine = "exemPLE de CHAîne";
let chaine_capitalisee = ma_chaine.to_uppercase();
print!("{}", chaine_capitalisee);
```
Ceci donnera comme résultat : "Exemple De Chaîne".

On peut également utiliser la méthode ```to_uppercase()``` pour capitaliser seulement la première lettre d'une chaîne. Voici un autre exemple :

```Rust
let ma_chaine = "exemPLE de CHAîne";
let prem_lettere_capitale = ma_chaine[..1].to_uppercase() + &ma_chaine[1..];
print!("{}", prem_lettere_capitale);
```
Ceci donnera comme résultat : "Exemple de chaîne".

## Plongeon Profond:

Dans les premières années de l'informatique, les ordinateurs fonctionnaient avec une quantité limitée de mémoire. Ainsi, pour économiser de l'espace, les programmeurs ont adopté des conventions de codage, dont la capitalisation des noms de variables et de fonctions.

Bien qu'il n'existe pas de règles strictes sur la façon de capitaliser une chaîne de caractères, il est important de suivre les conventions de codage de votre équipe ou de votre projet pour une meilleure lisibilité du code.

D'autres langages de programmation ont des méthodes similaires pour capitaliser une chaîne de caractères, tels que ```capitalize()``` en Python ou ```toUpper()``` en JavaScript.

## Voir Aussi:

- Pour en savoir plus sur la capitalisation des chaînes de caractères en Rust, consultez la documentation officielle de Rust : https://doc.rust-lang.org/std/primitive.str.html#method.to_uppercase
- Pour en savoir plus sur les conventions de codage en programmation, lisez cet article : https://blog.wikimedia.org/2011/07/29/programming-styles/