---
title:                "Extraire des sous-chaînes"
html_title:           "Rust: Extraire des sous-chaînes"
simple_title:         "Extraire des sous-chaînes"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/rust/extracting-substrings.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi?
L'extraction de sous-chaînes est simplement le fait de séparer une chaîne de caractères en plusieurs parties plus petites, appelées sous-chaînes. Les programmeurs le font souvent pour traiter des chaînes de caractères de manière plus efficace et pour ne travailler qu'avec les parties pertinences de la chaîne.

## Comment faire:
Il existe plusieurs façons d'extraire des sous-chaînes en Rust, dont voici quelques exemples:

```Rust
let s = "Bonjour tout le monde";
let sous_chaine = &s[3..10];
println!("{}", sous_chaine); // Affiche "jour to"
```

Nous pouvons également extraire une sous-chaîne à partir d'un index spécifique jusqu'à la fin de la chaîne:

```Rust
let s = "Hello World!";
let sous_chaine = &s[6..];
println!("{}", sous_chaine); // Affiche "World!"
```

Et si nous souhaitons extraire une sous-chaîne à partir du début de la chaîne jusqu'à un index spécifique:

```Rust
let s = "Rust est génial!";
let sous_chaine = &s[..4];
println!("{}", sous_chaine); // Affiche "Rust"
```

## Deep Dive:
L'extraction de sous-chaînes est une opération courante dans la programmation, car elle permet de manipuler des données sous forme de chaînes de caractères de manière plus efficace. Cela peut être utile pour des choses comme la manipulation de texte, la recherche et le filtrage de données.

En termes d'implémentation, Rust utilise un type spécifique appelé "str" pour représenter les chaînes de caractères. Ce type possède des méthodes intégrées pour extraire des sous-chaînes, telles que "slice" qui est utilisée dans les exemples ci-dessus.

Il existe également d'autres méthodes pour extraire des sous-chaînes, telles que "split", qui sépare une chaîne en plusieurs parties en utilisant un délimiteur spécifique, et "chars", qui itère à travers chaque caractère de la chaîne.

## Voir aussi:
- [La documentation officielle de Rust pour l'extraction de sous-chaînes](https://doc.rust-lang.org/std/str/index.html#methods)
- [Un tutoriel vidéo sur l'extraction de sous-chaînes en Rust](https://www.youtube.com/watch?v=gfkTfcpWqAY)
- [Un article approfondi sur les méthodes d'extraction de sous-chaînes en Rust](https://www.samizdat.dev/extraction-of-substrings-in-rust/)