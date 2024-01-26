---
title:                "Calcul d'une date future ou passée"
date:                  2024-01-20T17:30:52.867315-07:00
model:                 gpt-4-1106-preview
simple_title:         "Calcul d'une date future ou passée"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/gleam/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## What & Why?
Calculer une date future ou passée consiste à déterminer une date en ajoutant ou soustrayant une durée à une date donnée. C'est utile pour planifier des événements, gérer des délais ou des abonnements.

## How to:
```gleam
import gleam/datetime.{Duration, add_duration}
import gleam/calendar.{from_iso_weekday, Date}

pub fn main() {
  let today = Date(year: 2023, month: 4, day: 1)
  let duration = Duration(days: 10)
  let future_date = today
    |> add_duration(duration)
  
  let past_duration = Duration(days: -10)
  let past_date = today
    |> add_duration(past_duration)
  
  future_date
  past_date
}

// Sample output:
// 2023-04-11 (10 days after April 1, 2023)
// 2023-03-22 (10 days before April 1, 2023)
```

## Deep Dive
La date et l'heure sont des concepts aussi vieux que l'humanité, mais leur gestion en programmation n'a débuté qu'avec les premiers ordinateurs. Gleam, un langage fonctionnel et typé statiquement, offre des bibliothèques, comme `gleam/datetime`, pour manipuler facilement ces données temporelles. Les alternatives incluent l'utilisation de librairies de bas niveau ou la programmation manuelle de la logique des calculs temporels, mais cela nécessite de comprendre la complexité des calendriers et des fuseaux horaires. `add_duration` simplifie cette tâche en cachant ces détails.
