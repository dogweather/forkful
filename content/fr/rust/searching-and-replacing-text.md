---
title:                "Rust: Recherche et remplacement de texte"
simple_title:         "Recherche et remplacement de texte"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/rust/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Pourquoi

La recherche et remplacer du texte est une tâche courante en programmation, et il est important de savoir comment le faire efficacement. En utilisant le langage de programmation Rust, vous pouvez facilement manipuler du texte grâce à diverses méthodes et fonctions.

## Comment faire

Tout d'abord, vous devez importer la bibliothèque standard de Rust pour travailler avec du texte: `std::prelude::*`. Ensuite, vous pouvez utiliser la méthode `replace` pour remplacer un motif donné dans une chaîne avec une autre chaîne. Voici un exemple de code qui remplace "bonjour" par "salut" dans une chaîne donnée:

```Rust
let phrase = "Bonjour, comment ça va?";
let nouvelle_phrase = phrase.replace("bonjour", "salut");
println!("{}", nouvelle_phrase);
```

La sortie de ce code sera "Salut, comment ça va?". Vous pouvez également utiliser les méthodes `find` et `replace_range` pour remplacer une sous-chaîne spécifique dans une chaîne.

## Plongée en profondeur

En plus des méthodes mentionnées ci-dessus, Rust a également une fonctionnalité appelée "patterns de capture". Cela vous permet de trouver des parties spécifiques d'une chaîne qui correspondent à un modèle donné et de les remplacer par une autre chaîne. Voici un exemple de code qui remplace les lettres `c` et `d` par `x` et `y` respectivement:

```Rust
let phrase = "abcdefg";
let nouvelle_phrase = regex::Regex::new("(c)(d)").unwrap().replace_all(&phrase, "(x)(y)");
println!("{}", nouvelle_phrase);
```

La sortie de ce code sera "abxefg". Vous pouvez également utiliser des expressions régulières pour des recherches et des remplacements plus complexes.

## Voir aussi

- [Documentation officielle de Rust sur les chaînes de caractères](https://doc.rust-lang.org/std/string/index.html)
- [Guide de programmation sur la manipulation de text en Rust](https://www.excelsior-cjh.com/blog/programmation/rust/rust-manipulation-du-texte/)
- [Développer un programme de recherche et remplacement avec Rust](https://www.asciiarmor.com/post/text_manipulation_in_rust/)