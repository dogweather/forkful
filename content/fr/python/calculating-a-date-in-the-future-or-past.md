---
title:                "Calculer une date dans le futur ou le passé"
html_title:           "Python: Calculer une date dans le futur ou le passé"
simple_title:         "Calculer une date dans le futur ou le passé"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/python/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Qu'est-ce et Pourquoi?

Calculer une date dans le futur ou le passé concerne de déterminer une date à venir ou précédente en ajoutant ou en soustrayant un certain intervalle à une date spécifiée. Les programmeurs le font pour des tâches variées comme la planification d'événements, les rappels, la gestion des délais d'attente, etc.

## Comment Faire:

```Python 
from datetime import datetime, timedelta

# Pour le futur
dans_5jours = datetime.now() + timedelta(days=5)
print("Dans 5 jours: ", dans_5jours)

# Pour le passé
il_y_a_5jours = datetime.now() - timedelta(days=5)
print("Il y a 5 jours: ", il_y_a_5jours)
```
*_Sample Output_*
```
Dans 5 jours: 2023-03-20 11:23:10.435758
Il y a 5 jours: 2023-03-20 11:23:11.438123
```

## Deep Dive

1. _In contexte historique_: En programmation, nous devons souvent calculer des dates relatives. Python a simplifié cette tâche avec le module datetime.

2. _Alternatives_: Vous pouvez également utiliser `dateutil.relativedelta`, qui offre plus de flexibilité, permettant d'ajouter ou de soustraire des années, des mois, etc.

3. _Détails d'implémentation_: `timedelta` représente une durée, la différence entre deux dates ou heures. Il a plusieurs paramètres (jours, secondes, microsecondes, millisecondes, minutes, heures, semaines), tous facultatifs et par défaut à zéro.

## Voir Aussi

[Documentation officielle Python](https://docs.python.org/fr/3/library/datetime.html): Comprend des informations détaillées et des exemples supplémentaires sur le module datetime et ses fonctionnalités.

[Tutoriel Python Datetime](https://www.w3schools.com/python/python_datetime.asp): Un tutoriel simple et facile à comprendre sur l'utilisation de datetime en Python.

[Poste StackOverflow](https://stackoverflow.com/questions/546321/how-do-i-calculate-the-date-six-months-from-the-current-date-using-the-datetime): Une question pertinente sur StackOverflow discutant de la façon de calculer la date à partir de 6 mois à partir de la date actuelle.