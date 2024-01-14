---
title:    "Python: Obtenir la date actuelle"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/python/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Pourquoi

La date est un élément fondamental de notre vie quotidienne, et les ordinateurs n'échappent pas à cette règle. Avec Python, il est possible d'obtenir la date actuelle de différentes manières, ce qui peut s'avérer utile pour de nombreuses tâches de programmation.

## Comment faire

La méthode la plus simple pour obtenir la date actuelle en Python est d'utiliser le module `datetime`. Il suffit d'importer ce module dans votre code et d'utiliser la fonction `date.today()` pour obtenir la date actuelle au format `année-mois-jour`. Voici un exemple de code :

```Python
import datetime

aujourd'hui = datetime.date.today()

print(aujourd'hui)
```

Lorsque vous exécutez ce code, vous devriez obtenir une sortie similaire à ceci : `2020-11-24`. Cette méthode est rapide et efficace pour obtenir la date actuelle, mais elle ne permet pas de personnaliser le format de la date.

Si vous souhaitez avoir un meilleur contrôle sur le format de la date, vous pouvez utiliser la fonction `strftime()` du module `datetime`. Cette fonction permet de convertir la date en une chaîne de caractères personnalisée en utilisant des codes spécifiques. Voici un exemple de code :

```Python
import datetime

aujourd'hui = datetime.date.today()

format = "%d/%m/%Y"

date_formattee = aujourd'hui.strftime(format)

print(date_formattee)
```

La sortie sera alors sous la forme `24/11/2020`, selon le format choisi. Vous pouvez trouver la liste complète des codes de formatage sur la documentation officielle de Python.

## Plongez un peu plus profond

Si vous souhaitez aller plus loin dans la manipulation des dates en Python, vous pouvez également utiliser le module `calendar`. Ce module propose des fonctions pour créer des calendriers personnalisés, calculer le jour de la semaine pour une date donnée, ou encore vérifier si une année est bissextile. Voici un exemple de code pour calculer le nombre de jours dans un mois spécifique :

```Python
import calendar

mois = 11 # novembre
annee = 2020

nombre_jours = calendar.monthrange(annee, mois)[1]

print(nombre_jours)
```

La sortie sera `30`, car novembre a 30 jours en 2020. Vous pouvez également utiliser le module `time` pour obtenir l'heure actuelle et la convertir en différentes unités de temps (secondes, minutes, heures) pour une utilisation dans vos programmes.

## Voir aussi

- Documentation officielle de Python : https://docs.python.org/fr/3/library/datetime.html
- Tutoriel sur les dates en Python : https://realpython.com/python-date-time/
- Tutoriel sur le module `calendar` : https://www.programiz.com/python-programming/datetime/calendar