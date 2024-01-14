---
title:    "Rust: Affichage du débogage"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/rust/printing-debug-output.md"
---

{{< edit_this_page >}}

## Pourquoi

Imprimer des sorties de débogage est un outil précieux pour les programmeurs, que ce soit pour comprendre le fonctionnement d'un code complexe ou pour résoudre rapidement des erreurs.

## Comment faire

Pour imprimer des sorties de débogage en Rust, utilisez simplement la macro `println!()` suivi du texte ou de la variable que vous souhaitez afficher. Par exemple :

```Rust
let a = 10;
println!("La valeur de a est : {}", a);
```

Cela affichera "La valeur de a est : 10" dans la console. Vous pouvez également utiliser plusieurs variables dans une seule instruction `println!()` en les séparant par des virgules.

## Plongée en profondeur

La macro `println!()` accepte en fait un nombre quelconque d'arguments, ce qui permet d'effectuer des opérations plus avancées. Vous pouvez par exemple utiliser le debug trait `{:?}` pour afficher le contenu d'une structure ou d'un tableau.

```Rust
#[derive(Debug)]
struct Personne {
    nom: String,
    age: u8,
}

let personne = Personne {
    nom: String::from("Jean"),
    age: 30,
};

println!("La personne est : {:?}", personne);
```

Cela affichera "La personne est : Personne { nom: "Jean", age: 30 }". Vous pouvez également utiliser la fonction `format!()` pour formater des valeurs avant de les afficher.

## Voir aussi

- [La documentation officielle sur les macros de débogage en Rust](https://doc.rust-lang.org/std/macro.println.html)
- [Guide de débogage Rust pour les débutants](https://blog.logrocket.com/debugging-in-rust-for-beginners/)
- [Vidéo tutoriel sur le débogage en Rust](https://www.youtube.com/watch?v=-Z9Ok4Z5S8I)