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

## Pourquoi

Comparer deux dates est une tâche courante en programmation. Cela permet de vérifier si une date est antérieure, postérieure ou égale à une autre, ce qui peut être utile pour des tâches telles que la gestion de plannings ou le tri de données chronologiques.

## Comment faire

Pour comparer deux dates en Python, nous pouvons utiliser le module `datetime`. Tout d'abord, nous devons les convertir en objets datetime à l'aide de la méthode `strptime()` en spécifiant le format de date souhaité. Ensuite, nous pouvons utiliser les opérateurs de comparaison pour déterminer la relation entre les deux dates. Voici un exemple de code :

```
# Import du module datetime
import datetime

# Définition des dates à comparer
date1 = "20/06/2021"
date2 = "15/06/2021"

# Conversion en objets datetime avec le format "%d/%m/%Y"
date1 = datetime.datetime.strptime(date1, "%d/%m/%Y")
date2 = datetime.datetime.strptime(date2, "%d/%m/%Y")

# Comparaison
if date1 > date2:
    print("Date1 est postérieure à Date2")
elif date1 < date2:
    print("Date1 est antérieure à Date2")
else:
    print("Date1 est égale à Date2")

# Output : Date1 est postérieure à Date2
```

## Deep Dive

Dans Python, les dates sont représentées par des objets de la classe `datetime`. Cette classe possède plusieurs attributs tels que `year`, `month`, `day`, `hour`, `minute` et `second`, qui permettent de manipuler les dates et heures de manière plus détaillée. De plus, le module `datetime` offre également des méthodes pratiques pour effectuer des opérations telles que l'ajout ou la soustraction d'une certaine durée à une date.

## Voir aussi

- [Documentation du module datetime](https://docs.python.org/fr/3/library/datetime.html)
- [Tutoriel sur les dates en Python](https://www.tutorialspoint.com/python/time_date.htm)
- [Guide sur la manipulation de dates en Python](https://realpython.com/python-datetime/)