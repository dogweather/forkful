---
title:                "Rust: Impression de la sortie de debug"
programming_language: "Rust"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/rust/printing-debug-output.md"
---

{{< edit_this_page >}}

## Pourquoi

Lorsque vous développez en Rust, il est important d'avoir un code bien structuré et facile à comprendre. Cela peut sembler difficile, surtout lorsque vous rencontrez des bugs ou des erreurs dans votre code. C'est là qu'entre en jeu l'impression des messages de débogage.

## Comment faire

La première étape pour imprimer des messages de débogage en Rust est d'utiliser la macro `println!()`. Cette macro prend en paramètre une chaîne de format et les variables à afficher. Voici un exemple :

```Rust
let num = 42;

println!("Le nombre est : {}", num);
```

Ce code imprimera "Le nombre est : 42" dans la console.

Vous pouvez également utiliser la macro `eprintln!()` pour imprimer des messages d'erreur dans la console.

```Rust
eprintln!("Il y a eu une erreur !");
```

En plus de la macro `println!()`, il existe également la macro `dbg!()` qui imprime à la fois le message de débogage et la valeur de la variable passée en paramètre.

```Rust
let nom = "Marie";
dbg!(nom);
```

Ce code imprimera "nom = "Marie"" dans la console.

## Plongée en profondeur

Il existe également d'autres options pour afficher des messages de débogage en Rust, telles que la macro `format!()` qui renvoie une chaîne formatée au lieu de l'imprimer directement à la console. Vous pouvez également utiliser la bibliothèque de débogage `log` pour enregistrer des messages de débogage dans un fichier plutôt que de les imprimer à la console.

Il est également important de noter que vous pouvez utiliser des variables pour personnaliser votre message de débogage. Par exemple, si vous avez besoin de connaître la taille d'un vecteur dans votre code, vous pouvez utiliser la syntaxe suivante :

```Rust
let vec = vec![1, 2, 3, 4, 5];

dbg!(format!("La taille du vecteur est : {}", vec.len()));
```

Ce code imprimera "La taille du vecteur est : 5".

## Voir aussi

- [Documentation officielle de Rust sur l'impression des messages de débogage](https://doc.rust-lang.org/book/ch05-01-defining-structs.html#debugging-with-println-macros)
- [Guide complet de Rust pour les débutants](https://www.rust-lang.org/learn/get-started)
- [Chaîne YouTube "Rustacean Station" pour des tutoriels et des conseils sur Rust](https://www.youtube.com/channel/UCicRZGYtPkmVFFVAYaHVRyg)