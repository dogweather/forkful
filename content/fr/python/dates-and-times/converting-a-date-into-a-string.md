---
changelog:
- 2024-04-04, dogweather, edited
- 2024-04-04, gpt-4-0125-preview, translated from English
date: 2024-01-20 17:37:21.660960-07:00
description: "Comment faire : Python facilite la conversion des dates en cha\xEEnes\
  \ de caract\xE8res. Utilisez la m\xE9thode\u2026"
lastmod: '2024-04-04T02:02:35.820741-06:00'
model: gpt-4-0125-preview
summary: "Python facilite la conversion des dates en cha\xEEnes de caract\xE8res."
title: "Convertir une date en cha\xEEne de caract\xE8res"
weight: 28
---

## Comment faire :
Python facilite la conversion des dates en chaînes de caractères. Utilisez la méthode [`strftime`](https://docs.python.org/3/library/datetime.html#strftime-and-strptime-behavior) disponible sur les objets [date](https://docs.python.org/3/library/datetime.html#date-objects). Voici comment :

```Python
from datetime import datetime

# Obtenir la date et l'heure actuelles
now = datetime.now()

# La convertir en une chaîne de caractères dans le format : Mois jour, Année
date_string = now.strftime("%B %d, %Y")
print(date_string)  # Sortie : 29 mars 2023 (ou date actuelle)

# Format : AAAA-MM-JJ
iso_date_string = now.strftime("%Y-%m-%d")
print(iso_date_string)  # Sortie : 2023-03-29 (ou date actuelle)
```


### Comment je le fais

Voici comment j'obtiens une date au format [ISO 8601](https://www.w3.org/QA/Tips/iso-date) avec les informations de fuseau horaire :

```python
def datestamp() -> str:
    """ 
    La date et l'heure actuelles avec le fuseau horaire au format ISO.
    """
    return datetime.now().astimezone().isoformat()
```

#### Exemple de sortie :

```python
>>> datestamp()
'2024-04-04T01:50:04.169159-06:00'
```


## Exploration plus approfondie
Historiquement, la conversion entre dates et chaînes de caractères a toujours été fondamentale en programmation, dû au besoin de représenter les dates dans un format lisible par l'homme.

Les alternatives à `strftime` incluent l'utilisation de la méthode `isoformat` pour le format ISO 8601, ou des bibliothèques tierces comme `arrow` et `dateutil` qui offrent des options de parsing et de formatage plus flexibles.

D'un point de vue implémentation, `strftime` signifie "string format time" (formatage de temps en chaîne) et trouve ses origines dans la programmation en C. Le `strftime` de Python interprète des codes de format comme `%Y` pour l'année et `%m` pour le mois, permettant une personnalisation presque infinie.

## Voir aussi
Pour approfondir les fonctions de date et d'heure de Python :
- La documentation officielle de `datetime` de Python : https://docs.python.org/3/library/datetime.html
- Pour ceux intéressés par une liste complète des directives `strftime` : https://strftime.org/
- Pour explorer les bibliothèques tierces de date/heure :
  - Arrow : https://arrow.readthedocs.io/en/latest/
  - python-dateutil : https://dateutil.readthedocs.io/en/stable/
