---
date: 2024-01-20 17:34:12.320453-07:00
description: "How to: | Comment faire : Comparer des dates est crucial en programmation\
  \ depuis l'av\xE8nement des syst\xE8mes informatiques. Historiquement, les probl\xE8\
  mes de\u2026"
lastmod: '2024-04-05T22:51:11.375492-06:00'
model: gpt-4-1106-preview
summary: "| Comment faire : Comparer des dates est crucial en programmation depuis\
  \ l'av\xE8nement des syst\xE8mes informatiques. Historiquement, les probl\xE8mes\
  \ de comparaison de dates ont conduit au fameux bug de l'an 2000 (Y2K), o\xF9 la\
  \ repr\xE9sentation de l'ann\xE9e sur deux chiffres posait des probl\xE8mes. En\
  \ Python, la biblioth\xE8que `datetime` est un moyen standard de manipuler des dates.\
  \ Une alternative \xE0 `datetime` est la biblioth\xE8que `arrow`, qui offre une\
  \ interface plus simple pour certains. Pour comparer efficacement deux dates, Python\
  \ calcule la diff\xE9rence en timestamp (le nombre de secondes depuis l'\xE9poque\
  \ Unix) ce qui simplifie la soustraction et la comparaison."
title: Comparer deux dates
weight: 27
---

## How to: | Comment faire :
```python
from datetime import datetime

# Définir deux dates
date1 = datetime(2022, 1, 31)
date2 = datetime(2023, 3, 15)

# Comparer les deux dates
if date1 < date2:
    print("date1 est avant date2")
elif date1 > date2:
    print("date1 est après date2")
else:
    print("date1 et date2 sont identiques")

# Calculer la différence entre les dates
difference = date2 - date1
print("Différence:", difference.days, "jours")
```
Sortie:
```
date1 est avant date2
Différence: 408 jours
```

## Deep Dive | Plongée en profondeur
Comparer des dates est crucial en programmation depuis l'avènement des systèmes informatiques. Historiquement, les problèmes de comparaison de dates ont conduit au fameux bug de l'an 2000 (Y2K), où la représentation de l'année sur deux chiffres posait des problèmes. En Python, la bibliothèque `datetime` est un moyen standard de manipuler des dates. Une alternative à `datetime` est la bibliothèque `arrow`, qui offre une interface plus simple pour certains. Pour comparer efficacement deux dates, Python calcule la différence en timestamp (le nombre de secondes depuis l'époque Unix) ce qui simplifie la soustraction et la comparaison.

## See Also | Voir Aussi
- Documentation officielle de `datetime`: https://docs.python.org/3/library/datetime.html
- Bibliothèque `arrow`: https://arrow.readthedocs.io/
- Explication du bug de l'an 2000 (Y2K): https://fr.wikipedia.org/wiki/Bug_de_l%27an_2000
