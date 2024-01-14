---
title:                "Elixir: Obtenir la date actuelle"
simple_title:         "Obtenir la date actuelle"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elixir/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Pourquoi

Obtenir la date actuelle peut sembler une tâche simple, mais il est toujours important de savoir comment le faire en tant que développeur Elixir. Dans cet article, nous allons explorer les différentes façons de récupérer la date actuelle en utilisant Elixir.

## Comment faire

Pour récupérer la date actuelle en Elixir, nous pouvons utiliser la fonction `Date.utc_today/0` qui renvoie un objet `Date` représentant le jour actuel en temps universel coordonné (UTC). Il est important de noter que cette fonction renvoie la date au format `YYYY-MM-DD` en tant que chaîne de caractères.

```Elixir
iex> Date.utc_today()
~D[2020-12-14]
```

Nous pouvons également utiliser la fonction `DateTime.utc_now/0` pour obtenir la date et l'heure actuelles en UTC.

```Elixir
iex> DateTime.utc_now()
~U[2020-12-14 11:22:24.175270Z]
```

Nous pouvons également formater la date et l'heure renvoyées en utilisant les fonctions `strftime/2` et `strptime/3`.

```Elixir
iex> Date.utc_today() |> strftime("%A, %B %d, %Y")
"Monday, December 14, 2020"

iex> DateTime.utc_now() |> strptime("%B %d, %Y %I:%M %p")
{:ok,
 ~U[2020-12-14 08:22:24.180860],
 "~B %d, %Y %H:%M %Z", 14}
```

## Plongée en profondeur

Maintenant que nous savons comment obtenir la date actuelle en utilisant Elixir, nous pouvons également explorer d'autres fonctionnalités telles que la comparaison de dates, la manipulation de dates et la gestion des fuseaux horaires.

Par exemple, nous pouvons utiliser l'opérateur `<` pour comparer deux dates et déterminer si l'une est antérieure à l'autre.

```Elixir
iex> ~D[2020-12-14] < ~D[2020-12-15]
true
```

De plus, nous pouvons utiliser les fonctions `Date.add/2` et `Date.diff/2` pour ajouter ou soustraire des jours à une date et pour calculer la différence en jours entre deux dates.

```Elixir
iex> Date.add(~D[2020-12-14], 5)
~D[2020-12-19]

iex> Date.diff(~D[2020-12-14], ~D[2020-12-10])
4
```

En ce qui concerne les fuseaux horaires, Elixir dispose d'une bibliothèque appelée `tzdata` qui gère les informations sur les fuseaux horaires. Nous pouvons utiliser la fonction `timex` pour convertir une date dans un fuseau horaire spécifique.

```Elixir
iex> DateTime.utc_now() |> Timex.to_datetime("America/New_York")
~U[2020-12-14 06:22:24.175270Z]
```

## Voir aussi

Pour en savoir plus sur la manipulation des dates en Elixir, consultez les ressources suivantes :

- [Documentation officielle Elixir sur les dates](https://hexdocs.pm/elixir/DateTime.html)
- [Documentation officielle Timex sur les dates et les heures](https://hexdocs.pm/timex/api-reference.html)
- [Documentation officielle tzdata sur les fuseaux horaires](https://hexdocs.pm/tzdata/readme.html)

Maintenant, vous êtes prêt à manipuler les dates en Elixir de manière efficace et précise !