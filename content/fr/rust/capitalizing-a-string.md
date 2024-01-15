---
title:                "Majuscules d'une chaîne de caractères"
html_title:           "Rust: Majuscules d'une chaîne de caractères"
simple_title:         "Majuscules d'une chaîne de caractères"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/rust/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous avez déjà travaillé avec des chaînes de caractères en Rust, vous avez peut-être rencontré des situations où vous deviez capitaliser une chaîne de caractères. Peut-être que vous vouliez afficher un message en majuscules ou peut-être que vous deviez vérifier si une chaîne de caractères était entièrement en majuscules. Quelle que soit la raison, savoir comment capitaliser une chaîne de caractères en Rust est une compétence utile à avoir.

## Comment faire

Pour capitaliser une chaîne de caractères en Rust, il suffit d'utiliser la méthode `to_uppercase()` de la structure `String`. Voici un exemple de code :

```rust
let my_string = String::from("hello world");
let capitalized_string = my_string.to_uppercase();

println!("{}", capitalized_string); // affiche "HELLO WORLD"
```

Comme vous pouvez le voir, en utilisant la méthode `to_uppercase()`, nous obtenons une nouvelle `String` avec toutes les lettres de notre chaîne d'origine en majuscules.

Il est également possible de capitaliser une chaîne de caractères en utilisant la méthode `to_uppercase()` directement sur une référence mutable vers la chaîne d'origine, ce qui économise de la mémoire et du temps de traitement. Voici un exemple de code :

```rust
let mut my_string = String::from("hello world");
my_string.to_uppercase();

println!("{}", my_string); // affiche "HELLO WORLD"
```

Il est important de noter que la méthode `to_uppercase()` ne fonctionnera que sur les chaînes de caractères composées uniquement de caractères ASCII. Si vous avez besoin de capitaliser une chaîne de caractères avec des caractères non-ASCII, vous devrez utiliser la méthode `to_uppercase()` de la structure `OsString`. Voici un exemple de code :

```rust
use std::ffi::OsString;

let my_string = OsString::from("héllø wørld");

println!("{}", my_string.to_string_lossy().chars().flat_map(|c| c.to_uppercase()).collect::<String>()); // affiche "HÉLLØ WØRLD"
```

## Plongée en profondeur

Maintenant que vous savez comment capitaliser une chaîne de caractères en Rust, vous pouvez également être intéressé par la façon dont cette méthode fonctionne en interne.

En réalité, la méthode `to_uppercase()` utilise la table de conversion Unicode pour convertir chaque caractère ASCII en son équivalent majuscule. Si vous essayez de capitaliser une chaîne de caractères contenant des caractères non-ASCII, la méthode utilise la bibliothèque système pour effectuer la conversion. Cela peut sembler complexe, mais grâce à cela, la méthode `to_uppercase()` fonctionne également avec des caractères dans différentes langues.

## Voir aussi

Maintenant que vous savez comment capitaliser une chaîne de caractères en Rust, vous pouvez également être intéressé par les sujets suivants :

- [Documentation officielle sur la structure `String` en Rust](https://doc.rust-lang.org/std/string/struct.String.html)
- [Table de conversion Unicode](https://en.wikipedia.org/wiki/Unicode_equivalence)
- [Documentation officielle sur la méthode `to_uppercase()` en Rust](https://doc.rust-lang.org/std/string/struct.String.html#method.to_uppercase)