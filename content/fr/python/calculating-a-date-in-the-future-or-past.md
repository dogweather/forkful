---
title:                "Python: Calculer une date dans le futur ou le passé."
programming_language: "Python"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/python/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Pourquoi

L'une des fonctionnalités essentielles en programmation est la capacité de calculer une date dans le futur ou dans le passé. Que vous vouliez planifier une tâche ou vérifier un délai, savoir comment effectuer cette opération vous sera sûrement utile.

## Comment faire

```python 
# Imports
import datetime
# Input date
date = datetime.date(2021, 10, 17)
# Calculating one week in the future
one_week = datetime.timedelta(weeks=1)
future_date = date + one_week
print(future_date)

# Output: 2021-10-24
```

Pour calculer une date dans le futur, nous pouvons utiliser le module datetime de Python. Le premier pas consiste à importer le module, puis nous pouvons entrer la date que nous voulons calculer. Dans cet exemple, nous avons défini notre date en tant que 17 octobre 2021. Ensuite, nous utilisons la méthode timedelta pour spécifier l'intervalle de temps que nous voulons ajouter. Dans ce cas, nous avons défini une semaine. Enfin, nous ajoutons l'intervalle à notre date d'origine et imprimons le résultat.

Pour calculer une date dans le passé, le processus est similaire, à l'exception de l'utilisation de l'opérateur de soustraction pour soustraire l'intervalle de temps à la date d'origine.

## Deep Dive

Le module datetime de Python offre une variété de méthodes pour manipuler les dates et les heures. Voici quelques-unes des méthodes les plus utiles pour calculer des dates dans le futur ou dans le passé :

- timedelta(days, seconds, microseconds, milliseconds, minutes, hours, weeks) : Cette méthode crée un intervalle de temps spécifié par les arguments et peut être utilisée pour ajouter ou soustraire cet intervalle à une date.
- replace(year, month, day) : Cette méthode remplace une partie spécifique d'une date. Par exemple, si nous voulons calculer la même date que l'année dernière, nous pouvons utiliser replace (year=2020).
- fromisoformat(date_string) : Cette méthode crée un objet date à partir d'une chaîne de caractères au format ISO (AAAA-MM-JJ).
- weekday() : Cette méthode renvoie le jour de la semaine correspondant à une date donnée, en commençant par 0 pour lundi et en finissant par 6 pour dimanche.

En utilisant ces méthodes et d'autres, nous pouvons ajuster et manipuler facilement les dates selon nos besoins.

## Voir aussi
- [Documentation officielle de Python sur le module datetime](https://docs.python.org/fr/3/library/datetime.html)
- [Tutorialspoint - Datetime en Python](https://www.tutorialspoint.com/python/python_date_time.htm)
- [Real Python - Date and Time in Python](https://realpython.com/python-datetime/)
- [GeeksforGeeks - Python Datetime Module](https://www.geeksforgeeks.org/python-datetime-module/)

Merci d'avoir lu ! N'hésitez pas à explorer davantage les possibilités offertes par le module datetime de Python pour manipuler les dates et les heures de manière efficace. À bientôt !