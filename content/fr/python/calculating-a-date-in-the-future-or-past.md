---
title:                "Calcul d'une date dans le futur ou le passé"
html_title:           "Python: Calcul d'une date dans le futur ou le passé"
simple_title:         "Calcul d'une date dans le futur ou le passé"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/python/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Pourquoi
Il peut arriver que l'on ait besoin de calculer une date dans le futur ou dans le passé pour diverses raisons. Peut-être que vous planifiez un événement ou que vous retracez l'historique d'une série d'événements.

## Comment faire
Pour calculer une date dans le futur ou dans le passé en Python, on peut utiliser le module `datetime`. Voici un exemple de code pour calculer la date dans 100 jours à partir d'aujourd'hui :

```Python
from datetime import datetime, timedelta

today = datetime.today()
future_date = today + timedelta(days=100)

print("La date dans 100 jours sera :", future_date)
```

Cela affichera : `La date dans 100 jours sera : 2021-10-15 12:00:00`.

Si vous souhaitez calculer une date dans le passé, il suffit de changer l'opérateur `+` en `-` dans le code ci-dessus. Par exemple, pour obtenir la date il y a 30 jours, il suffit d'utiliser `- timedelta(days=30)`.

Il est également possible de spécifier la date à partir de laquelle calculer en utilisant la méthode `strptime()` de l'objet `datetime`. Voici un exemple :

```Python
from datetime import datetime, timedelta

date = datetime.strptime("2021-08-30", "%Y-%m-%d")
past_date = date - timedelta(days=15)

print("La date il y a 15 jours était :", past_date)
```

Cela affichera : `La date il y a 15 jours était : 2021-08-15 00:00:00`.

## Approfondissement
Le module `datetime` offre de nombreuses méthodes utiles pour manipuler et calculer des dates. Par exemple, la méthode `strftime()` permet de formater une date en une chaîne de caractères selon un modèle spécifié.

Il est également possible de calculer des dates en utilisant d'autres mesures de temps, par exemple des heures ou des minutes, en utilisant les arguments `hours` ou `minutes` dans la méthode `timedelta()`.

Pour plus d'informations sur le module `datetime` et ses fonctions, vous pouvez consulter la documentation officielle de Python.

## Voir aussi
- [Documentation officielle de Python sur le module datetime](https://docs.python.org/fr/3/library/datetime.html)
- [Calculer des dates avec Python (en anglais)](https://realpython.com/python-datetime/)