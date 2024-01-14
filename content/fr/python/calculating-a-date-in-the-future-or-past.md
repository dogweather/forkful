---
title:                "Python: Calculer une date dans le futur ou le passé."
simple_title:         "Calculer une date dans le futur ou le passé."
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/python/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Pourquoi
Calculer une date dans le futur ou le passé peut être utile pour planifier des événements, des voyages ou des tâches à l'avance. Cela peut également aider à résoudre des problèmes de programmation liés aux dates et aux délais.

## Comment faire
Il existe plusieurs façons de calculer une date dans le futur ou le passé en utilisant Python. Voici quelques exemples en utilisant le module standard datetime :

```Python
# Importer le module datetime
import datetime

# Obtenir la date actuelle
now = datetime.datetime.now()

# Calculer une date dans le futur en ajoutant 10 jours à la date actuelle
future_date = now + datetime.timedelta(days=10)
print(future_date.strftime("%d/%m/%Y"))
# Output: 14/05/2021

# Calculer une date dans le passé en soustrayant 2 mois à la date actuelle
past_date = now - datetime.timedelta(weeks=8)
print(past_date.strftime("%d/%m/%Y"))
# Output: 03/03/2021

# Calculer la différence en jours entre deux dates
date_1 = datetime.datetime(2021, 5, 5)
date_2 = datetime.datetime(2021, 4, 10)
difference = date_1 - date_2
print(difference.days)
# Output: 25
```

## Plongée en profondeur
Le module datetime offre de nombreuses autres fonctions utiles pour travailler avec les dates et les délais. Par exemple, vous pouvez utiliser la méthode strptime() pour convertir une chaîne de caractères en un objet de date, ou utiliser strftime() pour convertir un objet de date en une chaîne de caractères selon un format spécifique.

De plus, le module calendar peut être utilisé pour générer des calendriers et des horaires, tandis que le module dateutil propose des outils pour travailler avec des fuseaux horaires et des conversions de dates.

N'hésitez pas à explorer ces modules et à découvrir toutes les fonctionnalités pour vous aider dans votre projet de calcul de dates.

## Voir aussi
- Documentation officielle de Python sur le module datetime : https://docs.python.org/fr/3/library/datetime.html
- Tutoriel sur le module dateutil : https://dateutil.readthedocs.io/en/stable/
- Documentation sur le module calendar : https://docs.python.org/fr/3/library/calendar.html