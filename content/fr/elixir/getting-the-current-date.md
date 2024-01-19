---
title:                "Obtenir la date actuelle"
html_title:           "Bash: Obtenir la date actuelle"
simple_title:         "Obtenir la date actuelle"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elixir/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?

Obtenir la date actuelle en programmation, cela signifie récupérer les informations en temps réel sur le jour, le mois, l'année, voire l'heure. Les développeurs le font souvent pour suivre les événements ou enregistrer des horodatages.

## Comment faire :

Voici comment vous pouvez obtenir la date actuelle en Elixir :

```Elixir
Date.utc_today()
```

Cela renvoie la date d'aujourd'hui en UTC.

Par exemple,

```Elixir
iex> Date.utc_today()
~D[2023-09-20]
```

## Plongée en profondeur

Depuis sa création, Elixir utilise le système de dates d'Erlang, qui est basé sur le POSIX time. Cependant, ce système a ses limites telles que le manque du support des zones horaires. Elixir 1.3 a introduit de nouvelles structures pour résoudre ces problèmes : Date, Time, DateTime, et NaiveDateTime.

Une alternative à `Date.utc_today()` serait d'obtenir le DateTime UTC actuel, puis de le convertir en date.

```Elixir
iex> DateTime.now("Etc/UTC") |> DateTime.to_date
~D[2023-09-20]
```

L'obtention de la date actuelle en Elixir peut être différente en fonction de l'utilisation du système d'exploitation et du fuseau horaire de l'utilisateur.

## Voir aussi

Pour plus de détails sur la manipulation de la date et de l'heure en Elixir, vous pouvez consulter la documentation officielle de Elixir [ici](https://hexdocs.pm/elixir/Date.html) et ce [guide pratique](https://www.amberbit.com/blog/2018/6/29/dates-and-time-in-elixir/).