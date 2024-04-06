---
date: 2024-01-20 17:32:07.796866-07:00
description: "Comment faire : Historiquement, comparer des dates en Bash \xE9tait\
  \ un peu laborieux avant l'introduction de certaines fonctionnalit\xE9s de `date`\
  \ et des\u2026"
lastmod: '2024-04-05T22:51:11.953454-06:00'
model: gpt-4-1106-preview
summary: "Historiquement, comparer des dates en Bash \xE9tait un peu laborieux avant\
  \ l'introduction de certaines fonctionnalit\xE9s de `date` et des calculs arithm\xE9\
  tiques."
title: Comparer deux dates
weight: 27
---

## Comment faire :
```Bash
#!/bin/bash

# Date 1 in YYYY-MM-DD
date1="2023-04-01"
# Date 2 in YYYY-MM-DD
date2="2023-04-15"

# Compare using date command
if [ "$(date -d "$date1" +%s)" -lt "$(date -d "$date2" +%s)" ]; then
    echo "La date1 est antérieure à la date2."
elif [ "$(date -d "$date1" +%s)" -gt "$(date -d "$date2" +%s)" ]; then
    echo "La date1 est postérieure à la date2."
else
    echo "Les deux dates sont identiques."
fi
```
Sortie attendue :
```
La date1 est antérieure à la date2.
```

## Plongée Profonde
Historiquement, comparer des dates en Bash était un peu laborieux avant l'introduction de certaines fonctionnalités de `date` et des calculs arithmétiques. Le format `+%s` convertit les dates en secondes depuis l'époque UNIX, facilitant la comparaison.

Comme alternative, `dateutils` est un ensemble d'outils puissant pour manipuler des dates. Il inclut `datediff` qui peut être utilisé pour comparer des dates directement.

En ce qui concerne les détails d'implémentation, prendre compte des fuseaux horaires et de l’heure d’été peut compliquer la comparaison directe des dates. Il convient donc de toujours comparer des dates normalisées en UTC pour éviter les pièges.

## Voir également
- GNU Coreutils: https://www.gnu.org/software/coreutils/manual/html_node/date-invocation.html
- Dateutils: http://www.fresse.org/dateutils/
- Bash Guide: https://mywiki.wooledge.org/BashGuide/Dates
