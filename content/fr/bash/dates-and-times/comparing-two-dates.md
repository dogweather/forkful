---
title:                "Comparer deux dates"
aliases: - /fr/bash/comparing-two-dates.md
date:                  2024-01-20T17:32:07.796866-07:00
model:                 gpt-4-1106-preview
simple_title:         "Comparer deux dates"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/bash/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?
Comparer deux dates, c'est vérifier si elles sont égales, savoir laquelle est antérieure ou postérieure. On le fait souvent pour gérer des échéances, trier des événements ou valider des durées.

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
