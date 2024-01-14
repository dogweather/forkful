---
title:    "Rust: Trouver la longueur d'une chaîne de caractères."
keywords: ["Rust"]
---

{{< edit_this_page >}}

## Pourquoi

Dans cet article, nous allons parler d'un aspect fondamental de la programmation Rust : trouver la longueur d'une chaîne de caractères. Que vous soyez un développeur débutant ou expérimenté, comprendre comment trouver la longueur d'une chaîne de caractères peut être très utile dans la résolution de problèmes et l'amélioration de vos compétences en programmation.

## Comment faire

La première étape pour trouver la longueur d'une chaîne de caractères en Rust est de comprendre la structure de données de la chaîne. En Rust, une chaîne de caractères est un type de données composé de zéro ou plusieurs caractères. Pour trouver la longueur d'une chaîne de caractères, nous allons utiliser la méthode `len()`. Examinons un exemple :

```Rust
let ma_chaine = String::from("Bonjour!");
let longueur = ma_chaine.len();

println!("La longueur de la chaîne est : {}", longueur);
```

Output:

```
La longueur de la chaîne est : 8
```

Dans cet exemple, nous créons une variable `ma_chaine` qui contient la chaîne "Bonjour!", puis nous utilisons la méthode `len()` pour trouver sa longueur. Enfin, nous imprimons la longueur à l'aide de `println!`.

La méthode `len()` peut également être utilisée sur une chaîne de caractères littérale :

```Rust
let longueur = "Bonjour!".len();

println!("La longueur de la chaîne est : {}", longueur);
```

## Plongée en profondeur

Maintenant que nous savons comment trouver la longueur d'une chaîne de caractères en Rust, examinons quelques points importants à connaître :

- La méthode `len()` renvoie la longueur en octets d'une chaîne de caractères. Cela peut différer du nombre de caractères en fonction de l'encodage utilisé.
- Si vous souhaitez trouver le nombre de caractères dans une chaîne de caractères Unicode, vous pouvez utiliser la méthode `chars()` puis compter le nombre d'éléments retournés.
- Si vous souhaitez trouver le nombre de graphèmes (unité d'écriture de la langue) dans une chaîne de caractères Unicode, vous pouvez utiliser la bibliothèque `unicode-segmentation`.

## Voir aussi

- [Documentation officielle Rust sur les chaînes de caractères](https://doc.rust-lang.org/std/string/index.html)
- [Chaînes de caractères en Rust : Un guide complet](https://www.abyssinia.io/rust/strings)
- [Unicode en Rust : Une introduction](https://www.unicode.org/reports/tr29/#Description_of_Grapheme_Cluster_Boundaries)