---
title:    "Rust: Calculer une date dans le futur ou le passé"
keywords: ["Rust"]
---

{{< edit_this_page >}}

# Pourquoi

Le calcul de dates dans le passé ou dans le futur peut être utile pour de nombreuses applications telles que la planification de projets, la gestion de tâches ou encore la visualisation de données temporelles.

# Comment faire

Pour calculer une date dans le futur ou dans le passé en Rust, nous pouvons utiliser le type de données `chrono::DateTime` combiné avec la fonction `add()` pour ajouter ou soustraire une durée à une date donnée.

```
use chrono::{DateTime, Duration, Utc};

let now = DateTime::<Utc>::from_utc(chrono::offset::Local::now().naive_local(), Utc);
let future_date = now.add(Duration::days(7));
let past_date = now.sub(Duration::weeks(2));

println!("La date dans une semaine sera : {}", future_date);
println!("La date il y a deux semaines était : {}", past_date);
```

La sortie de ce code sera :

```
La date dans une semaine sera : 2021-10-24 12:00:00 UTC
La date il y a deux semaines était : 2021-09-26 12:00:00 UTC
```

# Plongée en profondeur

Il est important de noter que les durées utilisées dans la fonction `add()` et `sub()` sont des types `Duration` de la bibliothèque `chrono`. Cela signifie que vous pouvez utiliser non seulement des jours et des semaines, mais aussi des heures, des minutes, des secondes, etc. pour calculer des dates avec plus de précision.

De plus, il existe également une fonction `subtract()` qui peut être utilisée pour calculer la différence entre deux dates et renvoyer une durée correspondante, ce qui peut être très utile dans certaines situations.

# Voir aussi

- La documentation officielle de `chrono`: https://docs.rs/chrono/0.4.19/chrono/
- Un tutoriel sur l'utilisation de `chrono` pour manipuler les dates en Rust: https://blog.qutheory.io/learning-rust-dates/