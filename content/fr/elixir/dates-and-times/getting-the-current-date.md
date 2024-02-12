---
title:                "Obtenir la date actuelle"
aliases:
- fr/elixir/getting-the-current-date.md
date:                  2024-02-03T19:09:22.252498-07:00
model:                 gpt-4-0125-preview
simple_title:         "Obtenir la date actuelle"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elixir/getting-the-current-date.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
  - 2024-02-05, dogweather, reviewed and corrected
lastmod:              2024-02-05
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?
Obtenir la date actuelle en Elixir implique d'accéder aux informations de date et d'heure du système, une tâche courante pour la journalisation, le marquage de données, ou toute fonctionnalité nécessitant la connaissance de la date courante. Cette opération est essentielle pour créer des applications sensibles au temps et pour des tâches comme la génération de rapports ou de horodatages dans une application web.

## Comment faire :
La bibliothèque standard d'Elixir, à travers le module `DateTime`, permet de récupérer la date et l'heure actuelles. Comme Elixir fonctionne sur la VM Erlang (BEAM), il tire parti des fonctionnalités Erlang sous-jacentes pour les opérations temporelles.

### Utiliser la bibliothèque standard d'Elixir
Elixir fournit la fonction `DateTime.utc_now/0` pour obtenir la date et l'heure actuelles en UTC.

```elixir
current_datetime_utc = DateTime.utc_now()
IO.inspect(current_datetime_utc)
```

**Exemple de sortie :**
```
~U[2024-02-05 19:58:40.925931Z]
```

Pour obtenir juste la date actuelle, vous pourriez extraire les composants année, mois et jour :

```elixir
{:ok, current_date} = Date.new(current_datetime_utc.year, current_datetime_utc.month, current_datetime_utc.day)
IO.inspect(current_date)
```

**Exemple de sortie :**
```
~D[2023-05-04]
```

### Utiliser la bibliothèque Timex
Pour des besoins en date-heure plus complexes, une bibliothèque tierce populaire appelée Timex peut être utilisée. Tout d'abord, ajoutez `Timex` à vos dépendances mix.exs :

```elixir
defp deps do
  [
    {:timex, "~> 3.7"}
  ]
end
```

Après avoir installé la dépendance (`mix deps.get`), vous pouvez utiliser Timex pour obtenir la date actuelle :

```elixir
current_date = Timex.today()
IO.inspect(current_date)
```

**Exemple de sortie :**
```
~D[2023-05-04]
```

Timex offre des fonctionnalités étendues pour la manipulation de la date et de l'heure, ce qui en fait un ajout puissant à vos applications Elixir, surtout lorsqu'il s'agit de gérer les fuseaux horaires, le formatage et l'analyse des dates et des heures.

En comprenant et en utilisant les capacités intégrées d'Elixir et la bibliothèque Timex, vous pouvez facilement travailler avec les dates et les heures dans vos applications Elixir, en adaptant l'expérience aux besoins de votre application avec précision et facilité.
