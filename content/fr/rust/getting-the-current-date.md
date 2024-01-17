---
title:                "Obtenir la date actuelle"
html_title:           "Rust: Obtenir la date actuelle"
simple_title:         "Obtenir la date actuelle"
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/rust/getting-the-current-date.md"
---

{{< edit_this_page >}}

# Quoi & Pourquoi?

Obtenir la date actuelle est une tâche courante pour les programmeurs Rust. Cela consiste à récupérer la date et l'heure actuelles sur le système d'exploitation. Les programmeurs utilisent souvent cette information pour enregistrer la date de création ou de modification d'un fichier, ou pour planifier l'exécution de certaines tâches en fonction de la date actuelle.

# Comment faire:

Pour obtenir la date actuelle en Rust, vous pouvez utiliser la méthode ```Utc::now()``` de la bibliothèque standard ```chrono```. Voici un exemple de code qui récupère la date actuelle et l'imprime ensuite:

```Rust
use chrono::Utc;

let current_date = Utc::now().format("%Y-%m-%d").to_string();
println!("La date actuelle est: {:?}", current_date);
```

Ce code va produire une sortie similaire à ceci:

```Console
La date actuelle est: 2021-05-17
```

# Plongée en profondeur:

Avant Rust 1.38, la méthode ```now()``` de la bibliothèque standard retournait un type de données ```SystemTime```, qui représentait la date et l'heure actuelles sous forme de nombre de secondes depuis l'époque Unix.

Depuis Rust 1.38, la méthode ```now()``` retourne un type de données ```DateTime<Utc>```, qui stocke la date et l'heure sous forme de structure avec différents champs pour les années, mois, jours, heures, minutes, secondes et millisecondes.

Une alternative à l'utilisation de la bibliothèque standard serait d'utiliser des bibliothèques tierces telles que ```time``` ou ```chrono-tz```. Ces bibliothèques offrent des fonctionnalités supplémentaires telles que la prise en compte des fuseaux horaires.

# À voir aussi:

- [Chrono documentation](https://docs.rs/chrono/latest/chrono/)
- [Time documentation](https://docs.rs/time/latest/time/)
- [Chrono-TZ documentation](https://docs.rs/chrono-tz/latest/chrono_tz/)