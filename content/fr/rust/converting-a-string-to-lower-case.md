---
title:                "Rust: Conversion d'une chaîne en minuscules"
programming_language: "Rust"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/rust/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Pourquoi

Il y a de nombreuses raisons pour lesquelles vous pourriez avoir besoin de convertir une chaîne de caractères en minuscules lors de l'écriture de code en Rust. Parfois, vous voudrez peut-être simplement uniformiser les données entrantes pour faciliter leur manipulation. D'autres fois, vous pourriez avoir besoin de le faire pour des raisons linguistiques, pour vous assurer que les mots sont conformes aux règles spécifiques des langues.

## Comment faire

La conversion d'une chaîne de caractères en minuscules en Rust est assez simple, grâce à la méthode to_lowercase(). Voici un exemple de code pour convertir une chaîne de caractères en minuscules et imprimer le résultat :

```Rust
let string = String::from("Je suis en RUST !");
let lowercase_string = string.to_lowercase();
println!("{}", lowercase_string); // imprime "je suis en rust !"
```

Comme vous pouvez le voir, l'appel de la méthode to_lowercase() sur une chaîne de caractères renvoie une nouvelle chaîne avec tous les caractères convertis en minuscules. Cela fonctionne non seulement pour les caractères alphabétiques, mais aussi pour les caractères spéciaux, tels que les accents.

## Plongée en profondeur

Si vous souhaitez découvrir plus en détail comment la méthode to_lowercase() fonctionne en interne, vous pouvez consulter la documentation officielle de Rust ou parcourir le code source du langage sur GitHub. En résumé, la méthode utilise un algorithme de conversion en minuscules Unicode pour gérer toutes les différentes règles de casse des langues.

## Voir aussi

- Documentation officielle de Rust sur la méthode to_lowercase() : https://doc.rust-lang.org/std/string/struct.String.html#method.to_lowercase

- Code source de Rust sur GitHub : https://github.com/rust-lang/rust

- Article sur la manipulation des chaînes de caractères en Rust : *Insert name of article in English*