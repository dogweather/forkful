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

Calculer une date dans le futur ou le passé en Python

## Quoi & Pourquoi? 
Calculer une date dans le futur ou le passé est une tâche courante en programmation, consistant à trouver une date en ajoutant ou soustrayant un certain nombre de jours, semaines, mois ou années à une date donnée. Les programmeurs utilisent cette fonctionnalité pour des raisons pratiques, telles que planifier des événements, gérer des tâches récurrentes ou effectuer des opérations dans des bases de données.

## Comment faire :
La bibliothèque standard de Python offre plusieurs modules pour manipuler les dates et les horaires, tels que `datetime` et `time`. Voici un exemple de code qui vous permettra de calculer une date dans le futur en ajoutant 10 jours à la date d'aujourd'hui :

```python
from datetime import date, timedelta

aujourdhui = date.today()
futur = aujourdhui + timedelta(days=10)
print(futur)
```

La sortie de ce code sera la date dans 10 jours à partir d'aujourd'hui au format `YYYY-MM-DD`.

Vous pouvez également soustraire des jours en utilisant la même méthode, ou utiliser d'autres unités de temps telles que `weeks` (semaines), `months` (mois) ou `years` (années) pour obtenir une date dans le futur ou le passé.

## Plongée en profondeur :
L'utilisation des dates dans les langages de programmation a évolué au fil du temps, des formats plus simples tels que les nombres de jours depuis une date de référence, jusqu'aux formats plus complexes et flexibles comme dans Python. D'autres bibliothèques tierces offrent également des fonctionnalités de manipulation des dates, telles que `python-dateutil`.

Si vous n'utilisez pas Python, d'autres langages de programmation offrent des moyens similaires de calculer des dates dans le futur ou le passé. Par exemple, en JavaScript, vous pouvez utiliser la méthode `setDate()` pour modifier une date existante.

## Voir aussi :
- [Documentation officielle de Python pour la manipulation des dates et horaires](https://docs.python.org/fr/3/library/datetime.html)
- [Module python-dateutil](https://dateutil.readthedocs.io/en/stable/)
- [Documentation JavaScript pour les objets Date](https://developer.mozilla.org/fr/docs/Web/JavaScript/Reference/Objets_globaux/Date)