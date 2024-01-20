---
title:                "Comparer deux dates"
html_title:           "Clojure: Comparer deux dates"
simple_title:         "Comparer deux dates"
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/rust/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Pourquoi et Quoi ?

Comparer deux dates consiste à déterminer si une date est antérieure, postérieure ou égale à une autre. Les programmeurs le font par exemple pour trier des événements ou pour déterminer l'écoulement du temps.

## Comment faire:

Rust nous permet de comparer deux dates de manière plus facile avec le package chrono. Voici comment le faire :

```Rust
// Ajoutez le paquet chrono dans votre Cargo.toml
chrono = "0.4"
```

Ensuite dans votre code :

```Rust
extern crate chrono;
use chrono::{DateTime, Utc};

fn main() {
    let date1: DateTime<Utc> = Utc::now();
    let date2: DateTime<Utc> = Utc::now();

    if date1 < date2 {
        println!("date1 est antérieure à date2");
    } else if date1 > date2 {
        println!("date1 est postérieure à date2");
    } else {
        println!("date1 est la même que date2");
    }
}
```

## Analyse détaillée

Avant l'utilisation courante de bibliothèques spécialisées, comparer deux dates était plus complexe et pouvait être plus sujet à erreur. Aujourd'hui, des outils comme Chrono rendent les choses plus simples et efficaces. Néanmoins, il est toujours possible de faire cette comparaison sans bibliothèque externe, mais cela complique le code et augmente le risque d'erreur.

Rust offre une couche d'abstraction pour traiter la complexité de la comparaison des dates (et du temps) en utilisant des principes issus du système Unix. En utilisant cette approche, chaque date est représentée en tant que nombre d'unités (souvent des secondes) à partir d'une date "époque" fixe, ce qui rend la comparaison aussi simple qu'une comparaison de nombres entiers.

## Voir Aussi

Pour plus d'informations, consultez ces liens utiles de la documentation de Rust :

- Documentation sur Chrono : [https://docs.rs/chrono/0.4.19/chrono/](https://docs.rs/chrono/0.4.19/chrono/)
- Documentation sur le type DateTime : [https://docs.rs/chrono/0.4.19/chrono/struct.DateTime.html](https://docs.rs/chrono/0.4.19/chrono/struct.DateTime.html)