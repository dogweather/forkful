---
title:                "Calcule de la date dans le futur ou le passé"
html_title:           "Rust: Calcule de la date dans le futur ou le passé"
simple_title:         "Calcule de la date dans le futur ou le passé"
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/rust/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Pourquoi

Nous avons tous besoin de planifier nos activités et rendez-vous à l'avance, que ce soit dans le présent ou dans le futur. Mais souvent, il faut aussi prendre en compte des dates passées pour comprendre et suivre notre emploi du temps. Dans cet article, nous allons expliquer comment utiliser Rust pour calculer des dates dans le passé et dans le futur, afin de faciliter la gestion de notre temps.

## Comment faire

Pour calculer une date dans le futur ou dans le passé en Rust, nous allons utiliser la bibliothèque standard de Rust appelée "chrono". Elle fournit des structures de données et des fonctions permettant la manipulation de dates et d'heures. Voici un exemple de code pour calculer une date dans le futur à l'aide de la fonction "naive_date" de chrono :

```Rust
use chrono::{naive::NaiveDate, Duration};

let current_date = NaiveDate::from_ymd(2021, 10, 11);
let future_date = current_date + Duration::days(30);

println!("Dans 30 jours, nous serons le {}", future_date);
```

La sortie de ce code sera "Dans 30 jours, nous serons le 2021-11-10". Comme vous pouvez le voir, nous avons utilisé la fonction "from_ymd" pour créer une date spécifique, puis nous avons ajouté une durée de 30 jours à cette date en utilisant la fonction "days" de chrono. Vous pouvez également utiliser la fonction "subtract" pour calculer une date dans le passé.

## Plongée en profondeur

La manipulation de dates en Rust peut sembler simple, mais il y a en réalité beaucoup de nuances à prendre en compte. Par exemple, la bibliothèque chrono suit le calendrier grégorien par défaut, mais elle propose également des options pour utiliser d'autres calendriers tels que le calendrier julien et le calendrier hégirien utilisé dans le calendrier musulman.

En outre, la bibliothèque prend également en charge la gestion des fuseaux horaires et la conversion entre les différentes zones horaires, ce qui peut être utile pour les applications nécessitant des dates dans différentes parties du monde.

Enfin, il est important de noter que la bibliothèque chrono fournit également des fonctions pour afficher les dates et les heures de manière plus lisible pour l'utilisateur final, en tenant compte des différentes conventions de formatage utilisées dans le monde.

## Voir aussi

Pour en savoir plus sur la manipulation de dates en Rust, consultez la documentation officielle de la bibliothèque chrono. Vous pouvez également explorer d'autres bibliothèques tierces telles que "date_time" et "rustime" pour des fonctionnalités supplémentaires.