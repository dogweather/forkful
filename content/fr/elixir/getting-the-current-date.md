---
title:                "Obtenir la date actuelle"
html_title:           "Elixir: Obtenir la date actuelle"
simple_title:         "Obtenir la date actuelle"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elixir/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Pourquoi

De nos jours, l'utilisation de la date et de l'heure actuelles est essentielle dans de nombreuses applications et systèmes informatiques. En utilisant la programmation en Elixir, obtenir la date actuelle peut être fait en un rien de temps grâce à la simplicité et la puissance du langage.

## Comment Faire

```elixir
Date.utc_today()
#=> ~D[2020-11-17]
```

Pour obtenir la date et l'heure exactes, nous pouvons utiliser la fonction `utc_today()` de la bibliothèque standard Elixir. Cela renvoie un objet Date avec la date en format ISO 8601. Si nous voulons afficher la date et l'heure en utilisant un fuseau horaire spécifique, nous pouvons utiliser la fonction `now()` et lui donner le fuseau horaire comme argument, comme ceci :

```elixir
DateTime.now("Europe/Paris")
#=> ~U[2020-11-17 15:42:30Z]
```

L'utilisation de la fonction `now()` renvoie un objet DateTime avec la date et l'heure actuelles dans le fuseau horaire spécifié. Nous pouvons également obtenir la date et l'heure actuelles en utilisant le fuseau horaire du système en utilisant la fonction `local_now()`, comme ceci :

```elixir
DateTime.local_now()
#=> ~U[2020-11-17 10:42:30Z]
```

## Plongée en Profondeur

Maintenant que nous savons comment obtenir la date et l'heure actuelles en utilisant Elixir, explorons brièvement comment cela fonctionne en coulisses. Les fonctions que nous avons utilisées (`utc_today()`, `now()` et `local_now()`) font partie d'un module appelé `DateTime` qui fait partie de la bibliothèque standard Elixir.

Ce module utilise une bibliothèque externe appelée `tzdata` pour gérer les fuseaux horaires et garantir que les dates et heures renvoyées sont précises et cohérentes. Il utilise également une fonctionnalité intégrée d'Elixir appelée `Erlang Calendar` pour manipuler les dates et les heures.

## Voir Aussi

- Documentation officielle d'Elixir pour le module `DateTime` : https://hexdocs.pm/elixir/Date.html
- Liste des fuseaux horaires pris en charge par la bibliothèque `tzdata` : https://hexdocs.pm/tzdata/readme.html
- Documentation officielle d'Erlang pour les opérations de calendrier : http://erlang.org/doc/man/calendar.html