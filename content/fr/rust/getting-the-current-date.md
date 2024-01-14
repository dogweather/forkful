---
title:                "Rust: Obtenir la date actuelle"
simple_title:         "Obtenir la date actuelle"
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/rust/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Pourquoi

La date est un aspect important de la programmation dans de nombreux langages, car elle est utilisée pour de nombreuses fonctionnalités telles que les horodateurs, les rappels et bien plus encore. En utilisant Rust, vous pouvez facilement obtenir la date actuelle pour vos projets grâce à sa bibliothèque standard riche en fonctionnalités.

## Comment Faire

Pour obtenir la date actuelle en Rust, vous pouvez utiliser la fonction ```Rust std::time::SystemTime::now()``` qui renvoie un objet ```Rust std::time::SystemTime``` représentant l'instant actuel.

Voici un exemple de code montrant comment utiliser cette fonction et afficher la date actuelle en console :

```Rust
use std::time::SystemTime;

fn main() {

    let now = SystemTime::now();
    println!("La date actuelle est : {:?}", now);
}
```

Output :

```
La date actuelle est : Ok(2021-09-30 12:00:01.249922+02:00)
```

Vous pouvez également formater la date pour qu'elle soit plus facile à lire à l'aide de la fonction ```Rust std::time::SystemTime::now()``` et des macros de formatage de Rust :

```Rust
use std::time::{SystemTime, UNIX_EPOCH};
use std::time::Duration;
use std::mem;

fn main() {

    let now = SystemTime::now();
    let seconds = match now.duration_since(UNIX_EPOCH) {
        Ok(n) => n.as_secs(),
        Err(_) => panic!("Erreur lors de la récupération du temps Unix"),
    };
    let current_date = format!("{} {}", seconds, mem::size_of_val(&now));
    println!("Le nombre de secondes depuis l'époque Unix et la taille du type SystemTime sont : {}", current_date);
}
```

Output :

```
Le nombre de secondes depuis l'époque Unix et la taille du type SystemTime sont : 1208176616
```

## Plongée Profonde

Maintenant que nous avons vu comment obtenir la date actuelle en Rust, il est important de comprendre comment cela fonctionne en profondeur. En règle générale, la date actuelle est obtenue à partir de l'horloge système de l'ordinateur. Cette horloge est basée sur un compteur qui s'incrémente chaque seconde depuis une date de référence appelée l'époque Unix. En utilisant ce compteur, le système peut calculer la date et l'heure actuelles.

Lorsque nous appelons la fonction ```Rust std::time::SystemTime::now()```, elle appelle ensuite la fonction système correspondante pour récupérer l'heure actuelle. Cette dernière peut varier en fonction du système d'exploitation, mais elle suit généralement le même principe décrit ci-dessus.

## Voir Aussi

- [Documentation Rust sur la gestion du temps](https://doc.rust-lang.org/std/time/index.html)
- [Guide de démarrage rapide Rust](https://www.rust-lang.org/learn/get-started)
- [Exemples de code Rust pour la récupération de la date](https://github.com/rust-lang/rust-by-example)