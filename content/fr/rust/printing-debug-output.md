---
title:                "Rust: Affichage de la sortie de débogage"
simple_title:         "Affichage de la sortie de débogage"
programming_language: "Rust"
category:             "Rust"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/rust/printing-debug-output.md"
---

{{< edit_this_page >}}

## Pourquoi

L'impression de débogage est un outil essentiel pour vérifier le bon fonctionnement de votre code Rust. Cela vous permet de voir les valeurs des variables à différents points d'exécution de votre programme, ce qui peut être très utile pour trouver et résoudre des erreurs.

## Comment faire

Pour imprimer des messages de débogage dans votre code Rust, vous pouvez utiliser la macro `dbg!()`. Cette macro prend en paramètre une expression et imprime à la fois la valeur de l'expression et son type. Voici un exemple d'utilisation :

```Rust
fn main() {
    let code = 200;
    dbg!(code);

    let message = "OK";
    dbg!(message);
}
```

Lorsque vous exécutez ce code, vous verrez les messages suivants dans votre console :

```
[src/main.rs:3] code = 200
[src/main.rs:6] message = "OK"
```

Cette sortie vous montre le nom de la source, la ligne et la colonne où le message de débogage a été imprimé, ainsi que le nom de la variable, sa valeur et son type.

Vous pouvez également utiliser la fonction `println!()` pour imprimer des messages de débogage. Cependant, cela nécessite de convertir vos variables en chaînes de caractères à l'aide de la fonction `format!()`. Voici un exemple :

```Rust
fn main() {
    let code = 200;
    println!("Code : {}", code);

    let message = "OK";
    println!("Message : {}", format!("{}", message));
}
```

La sortie sera identique à celle produite par la macro `dbg!()`.

## Plongée en profondeur

La macro `dbg!()` et la fonction `println!()` sont pratiques pour imprimer rapidement des messages de débogage. Cependant, si vous souhaitez des options plus avancées, vous pouvez utiliser le crate `log` qui fournit un ensemble de macros pour l'enregistrement de messages de débogage. Cela vous permettra de définir différents niveaux de débogage, de manière à ce que certains messages ne soient imprimés que dans certaines situations.

De plus, si vous travaillez sur un projet plus important, vous pouvez utiliser un outil comme `gdb` pour déboguer votre code Rust de manière plus approfondie. Cela vous permettra de mettre en pause l'exécution de votre programme à des points spécifiques et d'examiner en détail les valeurs de vos variables.

## Voir aussi

- [Documentation officielle Rust - Impression de débogage](https://doc.rust-lang.org/book/ch05-01-defining-structs.html#printing-structs)
- [Crate `log`](https://docs.rs/log/0.4.11/log/index.html)
- [Documentation de `gdb`](https://www.gnu.org/software/gdb/)