---
title:                "Obtenir la date actuelle"
html_title:           "Bash: Obtenir la date actuelle"
simple_title:         "Obtenir la date actuelle"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/python/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Qu'est-ce & Pourquoi ?
Obtenir la date actuelle est un processus qui donne la date du jour présent. Les développeurs utilisent cette fonction pour des tâches telles que les horodatages, le suivi des modifications et la planification des tâches.

## Comment faire :
Voici un exemple de code simple pour obtenir la date actuelle.

```Python
from datetime import date

aujourdhui = date.today()
print(aujourdhui)
```

En exécutant ce code, vous obtiendrez une sortie de ce genre :
    
```Python
2022-06-05
```

## Deep Dive
Historiquement, Python utilise le module `time` pour gérer les dates et l'heure, mais c'est un peu encombrant. Le module `datetime`, introduit dans Python 2.3, est beaucoup plus intuitif.

Il existe plusieurs alternatives pour obtenir la date actuelle dans Python. Outre `datetime.date.today()`, vous pouvez également utiliser `datetime.datetime.now()`. 

```Python
from datetime import datetime

maintenant = datetime.now()
print(maintenant)
```

Notez que `.now()` renvoie également l'heure courante.

Le module `datetime` de Python fait partie de la bibliothèque standard, il est donc tout à fait fiable. La fonction `.today()` renvoie un objet `datetime.date` avec le jour, le mois et l'année actuels.

## Voir Aussi
- Documentation officielle de Python sur le module datetime : https://docs.python.org/3/library/datetime.html
- Comment utiliser le module datetime dans Python (EN) : https://realpython.com/python-datetime/ 
- Autres alternatives pour obtenir la date et l'heure actuelles dans Python (EN) : https://stackoverflow.com/questions/415511/how-to-get-the-current-time-in-python