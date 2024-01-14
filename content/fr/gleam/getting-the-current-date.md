---
title:                "Gleam: Obtenir la date actuelle"
programming_language: "Gleam"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/gleam/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Pourquoi

Obtenir la date actuelle peut sembler être une tâche simple et sans importance dans un langage de programmation. Cependant, il est crucial pour de nombreux cas d'utilisation, tels que l'enregistrement de données, la planification de tâches ou simplement pour donner à l'utilisateur une meilleure expérience avec des dates formatées correctement.

## Comment faire

Pour obtenir la date actuelle en utilisant Gleam, nous pouvons utiliser la fonction `Time.now()` qui renvoie un `Result` contenant les informations de date et d'heure actuelles. Voici un exemple de code:

```Gleam
let result = Time.now()

case result {
  Ok(time) -> "Date actuelle: #{time}"
  Error(err) -> "Erreur: #{err}"
}
```

Lors de l'exécution de ce code, nous obtiendrons une sortie qui ressemblera à cela:

> Date actuelle: {year: 2021, month: 10, day: 20, hour: 14, minute: 30, second: 42}

Nous pouvons également formater la sortie en utilisant la fonction `Time.to_utc()` qui prend en paramètre le résultat de `Time.now()` et un fuseau horaire en tant que `String`. Voici un exemple de code:

```Gleam
let result = Time.now()
let timezone = "Europe/Paris"

case result {
  Ok(time) -> "Date actuelle en #{timezone}: #{Time.to_utc(time, timezone)}"
  Error(err) -> "Erreur: #{err}"
}
```

La sortie sera alors formatée en fonction du fuseau horaire spécifié, comme ceci:

> Date actuelle en Europe/Paris: {year: 2021, month: 10, day: 20, hour: 12, minute: 30, second: 42}

## Plongée en profondeur

En plongeant en profondeur dans la façon dont Gleam gère les informations de date et d'heure, nous pouvons voir qu'elle utilise le type `DateTime` pour stocker ces données. Ce type est basé sur des structures de données immuables, garantissant ainsi la sécurité et la précision des dates manipulées par le programme.

De plus, Gleam offre également des fonctions pour manipuler les dates, telles que `Time.add_days()` pour ajouter un certain nombre de jours à une date donnée, ou `Time.is_leap_year()` pour vérifier si une année est bissextile.

## Voir aussi

- La documentation officielle sur la manipulation des dates en Gleam (https://gleam.run/docs/std/time/)
- Un article sur la gestion des dates en Gleam (https://medium.com/@gleam_language/handling-dates-in-gleam-c3f255228566)
- Un forum communautaire pour poser des questions ou partager des connaissances sur Gleam (https://github.com/gleam-lang/gleam/discussions)