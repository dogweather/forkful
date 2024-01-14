---
title:    "Python: Obtenir la date actuelle"
keywords: ["Python"]
---

{{< edit_this_page >}}

## Pourquoi obtenir la date actuelle est important en programmation Python?

Lorsque vous travaillez sur des projets de programmation avec Python, vous devrez souvent utiliser la date actuelle dans votre code. Cela peut sembler trivial, mais connaître la date actuelle peut être utile dans de nombreuses situations, telles que la gestion du temps, les tâches planifiées et les enregistrements de données.

## Comment obtenir la date actuelle avec Python

Il existe plusieurs façons d'obtenir la date actuelle en utilisant Python. Voici un exemple de code simple utilisant le module `datetime` pour imprimer la date actuelle :

```Python
import datetime

now = datetime.datetime.now()
print(now)
```

Lorsque vous exécutez ce code, vous obtiendrez l'heure et la date actuelles au format `année-mois-jour heure:minute:seconde.microseconde`. Mais vous pouvez également formater la sortie pour afficher la date d'une manière plus lisible, voici un exemple :

```Python
import datetime

now = datetime.datetime.now()
print(now.strftime("%d/%m/%Y, %H:%M:%S"))
```

Cela affichera la date et l'heure actuelles au format `jour/mois/année, heure:minute:seconde`. Vous pouvez également utiliser le module `time` pour obtenir la date actuelle sous forme d'une estampille de temps (timestamp) en secondes depuis l'époque :

```Python
import time

now = time.time()
print(now)
```

## Deep Dive

En utilisant le module `datetime`, il est possible d'obtenir plus de détails sur la date actuelle, tels que le jour de la semaine, le mois et l'année. Voici un exemple de code qui utilise ces informations pour imprimer un message de salutation personnalisé en fonction du jour de la semaine :

```Python
import datetime

now = datetime.datetime.now()
day = now.strftime("%A")

if day == "Monday":
  print("Bonjour, c'est le début d'une nouvelle semaine !")
elif day == "Friday":
  print("Bonjour, c'est bientôt le week-end !")
else:
  print("Bonjour, c'est un jour ordinaire.")
```

Vous pouvez également utiliser le module `calendar` pour obtenir une vue du calendrier du mois en cours :

```Python
import calendar

month = calendar.month(datetime.datetime.now().year, datetime.datetime.now().month)
print(month)
```

## Voir aussi

- [Documentation officielle de Python pour le module `datetime`](https://docs.python.org/fr/3/library/datetime.html)
- [Tutorialspoint - Python Date & Time](https://www.tutorialspoint.com/python/python_date_time.htm)
- [Real Python - Working with Dates and Times Using the datetime Module](https://realpython.com/python-datetime/)