---
title:    "Elixir: Calculer une date dans le futur ou le passé"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

## Pourquoi

La manipulation de dates est une tâche fréquente en programmation, et Elixir offre de puissantes fonctionnalités pour cela. Que vous cherchiez à calculer une date dans le futur ou dans le passé, apprendre à utiliser ces fonctions peut grandement faciliter le développement de votre application.

## Comment faire

La librairie `Calendar` d'Elixir contient plusieurs fonctions utiles pour manipuler les dates. Voici quelques exemples de codes:

```Elixir
# Pour calculer une date dans le futur en utilisant une unité de temps spécifique:
iex> Calendar.DateTime.add!(~U[2020-01-01T00:00:00], 1, :week)
~U[2020-01-08T00:00:00]

# Pour calculer une date dans le passé en utilisant un intervalle de temps:
iex> Calendar.DateTime.subtract(~U[2020-01-01T00:00:00], {3, :days})
~U[2019-12-29T00:00:00]

# Pour obtenir le nombre de jours entre deux dates:
iex> Calendar.days_between(~D[2020-01-01], ~D[2020-01-05])
5
```

Il est également possible de manipuler les dates en utilisant le type `NaiveDateTime` qui ne prend pas en compte les fuseaux horaires. La documentation complète de la librairie `Calendar` peut être trouvée [ici](https://hexdocs.pm/elixir/Calendar.html).

## Plongée en profondeur

En plus des opérations de base, il existe des fonctions plus avancées pour calculer des dates dans le futur ou dans le passé. Par exemple, vous pouvez utiliser `DateTime.now/0` pour obtenir la date et l'heure actuelles et les utiliser pour calculer une date dans le futur ou dans le passé. Vous pouvez également utiliser `DateTime.to_unix/1` pour convertir une date en un timestamp Unix.

Pour une compréhension plus approfondie de la manipulation des dates en Elixir, consultez cet article de blog [ici](https://bob.so/blog/working-with-dates-in-elixir/).

## Voir aussi

- [Documentation d'Elixir sur la manipulation des dates](https://hexdocs.pm/elixir/Calendar.html)
- [Article de blog sur la manipulation des dates en Elixir](https://bob.so/blog/working-with-dates-in-elixir/)
- [Article de blog sur l'utilisation de librairies tierces pour la manipulation des dates en Elixir](https://ashleydawson.github.io/2017/elixir-date-time/)