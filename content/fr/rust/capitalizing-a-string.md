---
title:    "Rust: Majusculation d'une chaîne de caractères."
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/rust/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Pourquoi

La capitalisation d'une chaîne de caractères est une tâche courante en programmation, que ce soit pour l'affichage de données à l'utilisateur ou pour le traitement de données. Dans cet article, nous allons explorer comment capitaliser une chaîne en utilisant Rust, un langage de programmation qui offre une performance élevée et une sécurité au niveau système.

## Comment faire

Pour commencer, importez le module `str` qui contient les méthodes pour manipuler les chaînes de caractères.

```Rust
use std::str; 
```

Ensuite, vous pouvez utiliser la méthode `to_uppercase()` pour capitaliser une chaîne.

```Rust
let s = "bonjour";
let capitalized = s.to_uppercase();
println!("Chaîne de départ : {}", s); // Bonjour
println!("Chaîne capitalisée : {}", capitalized); // BONJOUR
```

Vous pouvez également utiliser la méthode `to_lowercase()` pour mettre en minuscule une chaîne, ou `to_titlecase()` pour la mettre en majuscule avec la première lettre de chaque mot en majuscule.

```Rust
let s = "hello";
let lowercased = s.to_lowercase();
let titlecased = s.to_titlecase();
println!("Chaîne de départ : {}", s); // Hello
println!("Chaîne en minuscule : {}", lowercased); // hello 
println!("Chaîne en majuscule avec la première lettre en majuscule : {}", titlecased); // Hello
```

Il est également possible de capitaliser uniquement la première lettre d'une chaîne en utilisant la méthode `to_ascii_uppercase()`, qui convertit uniquement les lettres ASCII en majuscule. Cela peut être utile si vous voulez conserver les accents et caractères spéciaux.

```Rust
let s = "école";
let first_capitalized_letter = str::to_ascii_uppercase(&s[0..1]);
println!("Première lettre capitalisée : {}", first_capitalized_letter); // É
```

## Plongée en profondeur

Maintenant que vous savez comment capitaliser une chaîne en utilisant Rust, voyons comment cela fonctionne sous le capot. En utilisant la méthode `to_uppercase()`, Rust convertit la chaîne en un type `UnicodeString` avant d'appliquer la capitalisation. Cela garantit que les caractères Unicode sont correctement gérés et que les majuscules sont bien respectées selon les règles linguistiques. 

De plus, la méthode `to_uppercase()` ne modifie pas la chaîne d'origine mais renvoie une nouvelle chaîne avec la capitalisation appliquée. Cela peut être utile si vous avez besoin de garder la chaîne d'origine et de la modifier plus tard.

## Voir aussi

- [Documentation officielle de Rust](https://doc.rust-lang.org/std/primitive.str.html)
- [Chaînes de caractères en Rust: Unicode, UTF-8, et ce que tout cela signifie](https://blog.thoughtram.io/string-immutable-mutable-what-the-heck/)
- [Tutoriel Rust pour débutants](https://www.rust-lang.org/fr/learn)