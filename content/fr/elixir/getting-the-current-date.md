---
title:    "Elixir: Obtenir la date actuelle"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

## Pourquoi

Les dates sont un aspect essentiel de la programmation, permettant de suivre le temps et de gérer les événements au fil du temps. Que vous construisiez une application de calendrier ou une fonctionnalité de rappel, il est important de pouvoir récupérer la date actuelle. Dans cet article, nous allons explorer comment obtenir la date actuelle en utilisant Elixir.

## Comment faire

Pour obtenir la date actuelle en Elixir, nous pouvons utiliser la fonction `DateTime.utc_now/0` du module `DateTime`. Cette fonction renvoie un objet `DateTime` représentant la date et l'heure actuelles sous forme de tuple. Nous pouvons ensuite utiliser des fonctions telles que `Date.year/1` et `Date.month/1` pour extraire l'année et le mois de cette date.

```Elixir
current_date = DateTime.utc_now()
# {year: 2021, month: 10, day: 13, hour: 15, minute: 30, microsecond: {238937, 6}, timezone: "Etc/UTC", utc_offset: 0}
year = Date.year(current_date)
# 2021
month = Date.month(current_date)
# 10
```

Nous pouvons également utiliser le module `Calendar` pour formater la date sous différents formats, tels que "DD/MM/YYYY" ou "MM/DD/YYYY".

```Elixir
formatted_date = Calendar.format("{0}/{1}/{2}", [month, Date.day(current_date), year])
# "10/13/2021"
```

## Plongée profonde

Il est important de noter que la fonction `DateTime.utc_now/0` renvoie l'heure actuelle en temps universel coordonné (UTC). Si nous voulons obtenir la date et l'heure dans un fuseau horaire spécifique, nous pouvons utiliser les fonctions `DateTime.from_utc/2` et `DateTime.to_utc/2`.

Par exemple, pour obtenir la date et l'heure actuelles dans le fuseau horaire "Europe/Paris", nous pouvons faire ce qui suit :

```Elixir
paris_tz = "Europe/Paris"   # Fuseau horaire "Europe/Paris"
paris_date = DateTime.from_utc(current_date, paris_tz)
# {year: 2021, month: 10, day: 13, hour: 17, minute: 30, microsecond: {238937, 6}, timezone: "Europe/Paris", utc_offset: 2}
```

Nous pouvons également utiliser les fonctions `Date` et `Calendar` avec cette date pour obtenir le jour de la semaine ou la date formatée selon le fuseau horaire spécifique.

## Voir aussi

- [Documentation Elixir pour le module DateTime](https://hexdocs.pm/elixir/DateTime.html)
- [Documentation Elixir pour le module Calendar](https://hexdocs.pm/elixir/Calendar.html)
- [Liste des fuseaux horaires supportés en Elixir](https://hexdocs.pm/tzdb/time_zones.html#list-of-time-zones)