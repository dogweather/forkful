---
date: 2024-01-20 17:32:54.639722-07:00
description: "How to / Comment faire : R\xE9sultat ."
lastmod: '2024-04-05T21:53:59.737482-06:00'
model: gpt-4-1106-preview
summary: ''
title: Comparer deux dates
weight: 27
---

## How to / Comment faire :
```Fish Shell
# Pour obtenir la date actuelle en secondes depuis l'époque (epoch)
set date_now (date +%s)

# Convertir une date spécifique en secondes (ex : 1er Janvier 2021)
set specific_date (date -d '2021-01-01' +%s)

# Comparer les dates pour voir laquelle est la plus récente
if test $date_now -gt $specific_date
    echo "La date actuelle est postérieure au 1er janvier 2021."
else
    echo "La date actuelle est antérieure ou égale au 1er janvier 2021."
end
```

Résultat :
```
La date actuelle est postérieure au 1er janvier 2021.
```

## Deep Dive / Exploration approfondie :
Historiquement, les systèmes Unix mesurent le temps en secondes depuis l'époque Unix: le 1er janvier 1970. Cette méthode est universelle mais ne prend pas en compte les fuseaux horaires et les changements d'heure saisonniers.

Les alternatives modernes comme les modules datetime en Python ou les librairies comme Moment.js en JavaScript facilitent le travail avec dates et heures, incluant la gestion des fuseaux horaires. 

En Fish, on utilise souvent `date` car il est intégré et suffisamment puissant pour la plupart des besoins. Cependant, comparer des dates complexes, comme des récurrences, peut nécessiter des outils externes ou des scripts plus avancés.

## See Also / Voir également :
- Documentation de Fish sur date : https://fishshell.com/docs/current/cmds/date.html
- Pour des manipulations de dates plus avancées : https://github.com/HowardHinnant/date
- Détails sur l'époque Unix : https://en.wikipedia.org/wiki/Unix_time
