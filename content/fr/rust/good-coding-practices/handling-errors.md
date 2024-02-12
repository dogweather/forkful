---
title:                "Gestion des erreurs"
aliases: - /fr/rust/handling-errors.md
date:                  2024-01-26T00:57:51.098076-07:00
model:                 gpt-4-1106-preview
simple_title:         "Gestion des erreurs"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/rust/handling-errors.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?

La gestion des erreurs consiste à gérer les choses quand elles ne se déroulent pas comme prévu. Les programmeurs le font pour gérer l'inattendu, en s'assurant que leurs programmes Rust sont robustes et ne se contentent pas de planter lorsqu'ils rencontrent un hic.

## Comment faire :

Rust gère les erreurs de deux manières principales : les erreurs récupérables et les erreurs irrécupérables. Examinons les deux.

Pour les erreurs récupérables, utilisez `Result<T, E>` :

```Rust
use std::fs::File;

fn open_file(filename: &str) -> Result<File, std::io::Error> {
    let f = File::open(filename);
    
    match f {
        Ok(file) => Ok(file),
        Err(e) => Err(e),
    }
}

fn main() {
    match open_file("hello.txt") {
        Ok(_file) => println!("Fichier ouvert avec succès."),
        Err(_e) => println!("Échec de l'ouverture du fichier."),
    }
}
```

Le résultat peut être soit "Fichier ouvert avec succès." soit "Échec de l'ouverture du fichier.", selon votre `hello.txt`.

Pour les erreurs irrécupérables, nous utilisons `panic!` :

```Rust
fn main() {
    // Ceci provoquera une panique du programme car le fichier n'existe probablement pas.
    let _f = File::open("nowhere.txt").unwrap();
}
```

Exécutez-le et vous verrez un message de panique. Votre programme s'arrête net.

## Plongée en profondeur

Historiquement, la gestion des erreurs en programmation a été compliquée. Rust fait bien les choses avec une distinction claire entre les erreurs récupérables et irrécupérables.

L'énumération `Result` est utilisée pour les erreurs récupérables. Elle est explicite - vous gérez la variante `Ok` ou `Err`. Vous avez aussi des méthodes comme `unwrap()` et `expect()`, mais ce sont des raccourcis rapides et risqués qui peuvent conduire à un `panic!`.

`panic!` est la manière dont Rust exprime qu'il s'est passé quelque chose de vraiment mauvais, et il ne peut pas gérer. C'est comme une erreur irrécupérable qui arrête immédiatement l'exécution. Un panic dans Rust est souvent ressenti avec des bugs que vous ne vous attendez pas à gérer, comme un index hors des limites d'un tableau.

La gestion des erreurs en renvoyant `Result` est préférable lorsque vous vous attendez à gérer des erreurs. C'est idiomatique en Rust, ce qui signifie que c'est la manière dont les développeurs Rust se sont mis d'accord pour faire les choses. Il y a aussi `Option<T>` pour les cas où une erreur se traduit simplement par quelque chose étant `None` plutôt que `Some(T)`. Il s'agit de s'attendre à l'inattendu sans crainte.

Des alternatives ? Bien sûr, vous pourriez utiliser d'autres crates de gestion des erreurs pour plus de fonctionnalités ou une utilisation plus ergonomique. Comme `anyhow` pour une gestion des erreurs simple, ou `thiserror` pour des erreurs dans le code de la bibliothèque.

## Voir aussi

Intéressé par une plongée plus profonde ? Voici où aller :

- [Le livre Rust sur la gestion des erreurs](https://doc.rust-lang.org/book/ch09-00-error-handling.html) - Un excellent endroit pour comprendre la philosophie de Rust en matière de gestion des erreurs.
- [Rust par l'exemple : la gestion des erreurs](https://doc.rust-lang.org/rust-by-example/error.html) - Des exemples interactifs pour se salir les mains.

Rappelez-vous, une bonne gestion des erreurs n'est pas juste une question de codage ; c'est prendre soin des utilisateurs de votre code. Bon codage !
