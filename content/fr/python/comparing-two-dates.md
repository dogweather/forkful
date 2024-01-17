---
title:                "Comparer deux dates"
html_title:           "Python: Comparer deux dates"
simple_title:         "Comparer deux dates"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/python/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi?
Comparer deux dates est un moyen pour les programmeurs de déterminer si deux dates sont les mêmes ou si l'une est antérieure ou ultérieure à l'autre. Cela peut être utile pour trier des données chronologiquement ou pour effectuer des calculs de durée entre deux événements.

## Comment faire:
Voici un exemple de code en Python pour comparer deux dates:
```
date1 = "2021-05-16"
date2 = "2021-05-20"

if date1 == date2:
    print("Les dates sont identiques")
elif date1 < date2:
    print("Date 1 est antérieure à Date 2")
else:
    print("Date 1 est ultérieure à Date 2")
```

Output:
```
Date 1 est antérieure à Date 2
```

## Plongée Profonde:
Les programmeurs ont toujours eu besoin de comparer des dates pour diverses tâches, comme trier des événements historiques ou effectuer des calculs de temps. Avant l'invention des ordinateurs, cela était souvent fait à la main en utilisant des calendriers papier. De nos jours, il existe également d'autres moyens de comparer des dates, tels que l'utilisation de modules spécifiques comme "datetime" en Python.

## Voir aussi:
Pour en savoir plus sur la gestion des dates en Python, vous pouvez consulter la documentation officielle sur les modules de temps et de date. Voici le lien pour la version actuelle (Python 3.9.5): https://docs.python.org/fr/3.9/library/time.html