---
title:    "Python: Comparer deux dates"
keywords: ["Python"]
---

{{< edit_this_page >}}

## Pourquoi

Il est commun en programmation de devoir comparer deux dates, que ce soit pour vérifier si un événement est antérieur ou postérieur à un autre, ou pour calculer la différence entre deux périodes de temps. Dans cet article, nous allons voir comment comparer deux dates en Python et les différentes méthodes disponibles pour le faire.

## Comment faire

Pour comparer deux dates en Python, nous allons utiliser le module `datetime`. Tout d'abord, nous devons créer deux objets `datetime` contenant les dates que nous souhaitons comparer :

```Python
from datetime import datetime

date1 = datetime(2020, 10, 15)
date2 = datetime(2020, 11, 20)
```

Une fois que nous avons nos deux dates, nous pouvons utiliser différents opérateurs pour les comparer. Par exemple, pour vérifier si `date1` est antérieure à `date2`, nous pouvons utiliser l'opérateur `<` :

```Python
date1 < date2
True
```

Pour vérifier si les dates sont égales, nous pouvons utiliser l'opérateur `==` :

```Python
date1 == date2
False
```

Nous pouvons également calculer la différence entre deux dates en utilisant l'opérateur `-` :

```Python
diff = date2 - date1
print(diff)
35 days, 0:00:00
```

Il est également possible de comparer les heures, les minutes et les secondes en utilisant les attributs correspondants des objets `datetime`.

## Plongée en profondeur

Si nous voulons comparer les dates avec une meilleure précision, nous pouvons également utiliser le module `dateutil`. Il inclut une fonction `parser` qui permet de convertir des chaînes de caractères en objets `datetime` :

```Python
from dateutil.parser import parse

date3 = parse('2020-10-15')
print(date3)
2020-10-15 00:00:00
```

Nous pouvons également utiliser des méthodes spécifiques de l'objet `datetime` pour comparer les années, les mois ou les jours :

```Python
date1.year < date2.year
True

date1.month == date2.month
False

date1.day == date2.day
True
```

## Voir aussi

Pour en savoir plus sur la manipulation de dates en Python, vous pouvez consulter ces ressources (en anglais) :

- [Documentation officielle de Python sur le module DateTime](https://docs.python.org/3/library/datetime.html)
- [Python Tutorial: Working with Dates and Times](https://www.datacamp.com/community/tutorials/python-datetime-tutorial)