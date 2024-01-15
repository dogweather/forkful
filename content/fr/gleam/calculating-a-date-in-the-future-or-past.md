---
title:                "Calcul d'une date dans le futur ou le passé"
html_title:           "Gleam: Calcul d'une date dans le futur ou le passé"
simple_title:         "Calcul d'une date dans le futur ou le passé"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/gleam/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Pourquoi

Vous êtes-vous déjà demandé comment calculer une date dans le futur ou dans le passé en utilisant du code ? Peut-être voulez-vous prédire un événement important ou simplement garder une trace de vos prochaines vacances. Dans cet article, nous allons découvrir comment le faire en utilisant Gleam.

## Comment faire

Pour calculer une date dans le futur ou dans le passé, nous allons utiliser la fonction `add` du module `Calendar` de Gleam. Voici un exemple de code pour calculer la date d'aujourd'hui dans 5 jours :

```Gleam
import gleam/calendar.{ add }

today = Calendar.today()
five_days_later = add(today, { days: 5 })
```

La fonction `add` prend deux arguments : la date de départ et une structure contenant les unités de temps que vous souhaitez ajouter à cette date. Dans cet exemple, nous ajoutons 5 jours à la date d'aujourd'hui. Vous pouvez également utiliser les unités `years`, `weeks`, `months` et `hours`.

Voici un autre exemple qui calcule la date d'il y a 1 mois :

```Gleam
one_month_ago = add(today, { months: -1 })
```

Et voici comment calculer la date d'il y a 2 ans et 3 semaines :

```Gleam
two_years_and_three_weeks_ago = add(today, { years: -2, weeks: -3 })
```

Vous pouvez voir que la syntaxe est simple et intuitive.

## Plongée dans les détails

La fonction `add` utilise la structure `Duration` pour représenter les unités de temps. Cela signifie que vous pouvez personnaliser votre calcul en utilisant d'autres unités telles que `minutes`, `seconds`, `milliseconds` et `microseconds`.

De plus, la fonction `add` prend également en compte les années bissextiles pour des calculs précis.

## Voir aussi

- La documentation officielle sur le module `Calendar` de Gleam : https://gleam.run/modules/gleam/calendar/latest/
- Un article sur la gestion des dates et heures en Gleam : https://dev.to/johanalbury/managing-dates-and-times-in-gleam-4nad