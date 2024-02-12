---
title:                "Calcul d'une date future ou passée"
aliases:
- /fr/python/calculating-a-date-in-the-future-or-past/
date:                  2024-01-20T17:31:47.372929-07:00
model:                 gpt-4-1106-preview
simple_title:         "Calcul d'une date future ou passée"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/python/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Quoi et Pourquoi ?
Calculer une date dans le futur ou le passé, c'est manipuler des dates pour en obtenir de nouvelles. Les programmeurs le font pour des raisons variées : planifications, rappels, ou suivis d'échéances.

## Comment faire :
```Python
from datetime import datetime, timedelta

# La date d'aujourd'hui
aujourdhui = datetime.now()

# Calculons une date dans 10 jours
dans_dix_jours = aujourdhui + timedelta(days=10)
print("Dans 10 jours, nous serons le :", dans_dix_jours.strftime("%d/%m/%Y"))

# Et une date il y a 30 jours
il_y_a_trente_jours = aujourdhui - timedelta(days=30)
print("Il y a 30 jours, c'était le :", il_y_a_trente_jours.strftime("%d/%m/%Y"))
```

## Plongée profonde
Historiquement, la manipulation de dates en programmation a toujours été un peu ardue, principalement à cause de la complexité du calendrier (mois de longueurs différentes, années bissextiles, etc.). Python simplifie le travail avec le module `datetime`, qui offre des outils pour ajouter ou soustraire du temps à des dates. Il y a des alternatives comme `dateutil` qui offrent encore plus de fonctionnalités, comme le calcul de deltas relatifs (e.g., "le dernier vendredi du mois"). Il est important de bien comprendre ces outils, car une mauvaise gestion des dates peut conduire à des bugs difficiles à repérer.

## Voir aussi
- Documentation officielle du module datetime : https://docs.python.org/3/library/datetime.html
- Bibliothèque dateutil pour des calculs de date avancés : https://dateutil.readthedocs.io/en/stable/
- PyPI (Python Package Index) pour découvrir d'autres bibliothèques de gestion de date : https://pypi.org/
