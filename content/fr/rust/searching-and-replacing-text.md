---
title:                "Rechercher et remplacer du texte"
html_title:           "Rust: Rechercher et remplacer du texte"
simple_title:         "Rechercher et remplacer du texte"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/rust/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous travaillez avec du texte dans vos projets de programmation, vous avez probablement rencontré des situations où vous deviez trouver et remplacer une certaine portion de texte. Il peut s'agir d'une faute de frappe ou d'un terme qui a changé et que vous devez mettre à jour dans tout votre code. Heureusement, Rust dispose de puissantes fonctionnalités pour effectuer des recherches et des remplacements de manière efficace et sans douleur.

## Comment faire

Pour effectuer une recherche et un remplacement de texte en Rust, nous utiliserons la fonction `replace()` de la bibliothèque standard `std::string`. Voici un exemple de code montrant comment rechercher et remplacer un mot spécifique dans une chaîne de caractères :

```rust
let mut text = String::from("Bonjour le monde!");
text.replace("Bonjour", "Salut");

println!("{}", text);  // Output: Salut le monde!
```

Dans cet exemple, nous avons créé une variable `text` contenant une chaîne de caractères et nous avons utilisé la fonction `replace()` pour remplacer le mot "Bonjour" par "Salut". La chaîne de caractères mise à jour est ensuite affichée à l'écran.

Vous pouvez également utiliser des outils plus avancés pour effectuer des recherches et des remplacements en utilisant des expressions régulières. La bibliothèque `regex` fournit des fonctionnalités pour travailler avec des expressions régulières en Rust. Voici un exemple de code utilisant cette bibliothèque :

```rust
use regex::Regex;

let text = "Hello world!";
let re = Regex::new("world").unwrap();
let replaced_text = re.replace(text, "universe");

println!("{}", replaced_text); // Output: Hello universe!
```

Dans cet exemple, nous avons importé la bibliothèque `regex` et utilisé sa fonction `new()` pour créer une expression régulière correspondant au mot "world". Nous avons ensuite utilisé la fonction `replace()` pour remplacer ce mot par "universe" dans la chaîne de caractères `text`.

## Plongée profonde

En plus de la fonction `replace()` de la bibliothèque `std::string` et de la bibliothèque `regex`, il existe d'autres options pour effectuer des recherches et des remplacements de texte en Rust. La bibliothèque `phf` propose des tables de hachage parfaites pour effectuer des remplacements de manière efficace dans de grandes quantités de texte. La bibliothèque `strsim` peut être utilisée pour comparer des chaînes de caractères et faciliter les remplacements basés sur la similarité.

En utilisant ces outils et en combinant différentes méthodes de recherche et de remplacement, vous pouvez créer des programmes en Rust qui peuvent gérer efficacement tout type de tâche de manipulation de texte.

## Voir aussi

- [La documentation sur les chaînes de caractères en Rust](https://doc.rust-lang.org/stable/std/primitive.str.html)
- [La bibliothèque `regex` pour les expressions régulières en Rust](https://crates.io/crates/regex)
- [La bibliothèque `phf` pour les tables de hachage parfaites en Rust](https://crates.io/crates/phf)
- [La bibliothèque `strsim` pour comparer des chaînes de caractères en Rust](https://crates.io/crates/strsim)