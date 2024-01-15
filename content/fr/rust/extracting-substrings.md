---
title:                "Extraction de sous-chaînes"
html_title:           "Rust: Extraction de sous-chaînes"
simple_title:         "Extraction de sous-chaînes"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/rust/extracting-substrings.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous avez déjà travaillé avec des chaînes de caractères en Rust, vous savez qu'il peut être utile à certains moments d'extraire des sous-chaînes, c'est-à-dire des parties spécifiques d'une chaîne plus longue. Que ce soit pour traiter des données textuelles ou pour manipuler des URL, la possibilité d'extraire des substrings peut être très pratique et vous permettra d'écrire un code plus efficace et plus propre.

## Comment faire

Voici comment extraire des sous-chaînes en Rust en utilisant les méthodes *slice* et *split_at* :

```Rust
let s = "Bonjour, comment ça va ?";
let sous_chaine = &s[8..15];
println!("{}", sous_chaine);

//devrait afficher "comment"
```

Ici, nous créons une chaîne de caractères avec la phrase "Bonjour, comment ça va ?", puis nous utilisons la fonction *slice* pour extraire un morceau de cette chaîne, de l'index 8 à l'index 15. L'indexage en Rust commence à 0, donc le premier caractère a l'index 0 et le dernier a l'index n-1. Nous utilisons également l'opérateur "&" pour prendre une référence à la sous-chaîne plutôt que de la copier. Cela nous permet d'économiser de la mémoire et d'améliorer les performances.

Vous pouvez également utiliser la méthode *split_at* pour extraire une sous-chaîne à partir d'un index spécifique :

```Rust
let s = "Rust est un langage de programmation moderne";
let (premiere_partie, seconde_partie) = s.split_at(4);
println!("{} {}", premiere_partie, seconde_partie);

//devrait afficher "Rust est un langage de programmation moderne"
```

Ici, nous utilisons la méthode *split_at* pour séparer la chaîne à l'index 4, ce qui nous donne deux sous-chaînes que nous pouvons ensuite afficher ensemble.

## Plongée en profondeur

Lorsque vous utilisez la méthode *slice*, il est important de comprendre que les sous-chaînes renvoyées font référence à la chaîne d'origine. Cela signifie que si vous modifiez la sous-chaîne, la chaîne d'origine sera également modifiée. Par exemple :

```Rust
let s = String::from("Hello World");
let sous_chaine = &s[0..6];
sous_chaine = sous_chaine.replace("Hello", "Bonjour");
println!("{}", s);

//devrait afficher "Bonjour World"
```

Dans cet exemple, nous avons tenté de remplacer la première partie de la chaîne d'origine avec la méthode *replace*. Cependant, cette méthode ne peut pas fonctionner sur une référence, car elle a besoin de modifier la chaîne elle-même. Cela nous conduit à une erreur de compilation.

Il est également important de noter que la méthode *split_at* ne renvoie pas une référence mais deux valeurs distinctes. Cela signifie que vous ne pourrez pas modifier la chaîne d'origine à travers les sous-chaînes renvoyées.

## Voir aussi

- Documentation sur les chaînes de caractères en Rust : https://doc.rust-lang.org/std/string/
- Tutoriel sur les slices en Rust : https://doc.rust-lang.org/rust-by-example/slice.html