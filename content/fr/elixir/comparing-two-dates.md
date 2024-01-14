---
title:                "Elixir: Comparaison de deux dates"
simple_title:         "Comparaison de deux dates"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elixir/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Pourquoi

Comparer deux dates peut être une tâche courante pour de nombreux programmes Elixir. Cela peut être utile pour vérifier la validité des données, planifier des tâches en fonction de la date ou simplement obtenir des informations sur la durée entre deux points dans le temps. Dans cet article, nous allons explorer comment comparer deux dates en utilisant Elixir.

## Comment Faire

Pour commencer, nous allons créer deux dates en utilisant le module `Date` d'Elixir. Nous pouvons utiliser la fonction `Date.utc_today()` pour obtenir la date actuelle et `Date.from_iso8601()` pour créer une date à partir d'une chaîne au format ISO 8601.

```
```elixir
date1 = Date.utc_today()
# => #Date<2021-08-19>

date2 = Date.from_iso8601("2021-08-24")
# => #Date<2021-08-24>
```

Maintenant que nous avons nos deux dates, nous pouvons les comparer en utilisant les opérateurs de comparaison `<`, `>`, `<=`, `>=` et `==` en fonction de nos besoins.

```
```elixir
date1 < date2
# => true

date1 > date2
# => false

date1 <= date2
# => true

date1 >= date2
# => false

date1 == date2
# => false
```

Dans cet exemple, nous pouvons voir que la date1 est inférieure à la date2, car elle est antérieure dans le temps. Nous pouvons également comparer des dates avec d'autres types de données, tels que des nombres ou des chaînes de caractères.

## Plongée Profonde

En utilisant l'opérateur `==`, nous pouvons également comparer si deux dates sont égales, ce qui peut sembler évident mais peut causer des problèmes si nous ne faisons pas attention au formatage des dates. Par exemple, si nous comparons une date avec une autre au format différent, même si les dates sont les mêmes, elles ne seront pas égales.

```
```elixir
date1 = Date.from_iso8601("2021-08-21")
# => #Date<2021-08-21>

date2 = Date.from_iso8601("08/21/2021")
# => #Date<2021-08-21>

date1 == date2
# => false
```

Pour éviter cela, nous pouvons utiliser la fonction `Date.is_same/2` qui prend en compte le formatage des dates lors de la comparaison.

```
```elixir
Date.is_same(date1, date2)
# => true
```

En plus de comparer des dates, nous pouvons également utiliser le module `Calendar` pour effectuer des calculs de temps, tels que la différence entre deux dates en jours, semaines, mois ou années.

```
```elixir
Calendar.days_between(date1, date2)
# => 3

Calendar.weeks_between(date1, date2)
# => 0.4285714285714286

Calendar.months_between(date1, date2)
# => 0.13043478260869565

Calendar.years_between(date1, date2)
# => 0.01015228426395939
```

Cela peut être utile pour planifier des tâches en fonction du temps écoulé ou pour obtenir des informations sur la durée entre deux événements.

## Voir Aussi

- Documentation sur le module `Date` : https://hexdocs.pm/elixir/Date.html
- Documentation sur le module `Calendar` : https://hexdocs.pm/elixir/Calendar.html
- Article sur le fonctionnement interne de la comparaison de dates en Elixir : https://evilmartians.com/chronicles/how-i-compare-dates-in-elixir