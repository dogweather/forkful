---
title:                "Obtenir la date actuelle"
date:                  2024-01-20T15:13:11.898168-07:00
html_title:           "C: Obtenir la date actuelle"
simple_title:         "Obtenir la date actuelle"
programming_language: "Bash"
category:             "Bash"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/bash/getting-the-current-date.md"
---

{{< edit_this_page >}}

## What & Why?
("Quoi et Pourquoi?")
La récupération de la date actuelle consiste à obtenir la date et l'heure précises à l'instant où la commande est exécutée. Les programmeurs ont besoin de cette information pour des tâches comme logger des événements, planifier des jobs, et horodater des fichiers.

## How to:
("Comment faire :")
Pour obtenir la date et l'heure :

```Bash
date
```

Sortie typique :

```plaintext
Mer 12 Jan 2023 15:04:12 CET
```

Pour un formatage personnalisé :

```Bash
date "+%Y-%m-%d %H:%M:%S"
```

Sortie personnalisée :

```plaintext
2023-01-12 15:04:12
```

## Deep Dive
("Plongée en Profondeur")
La commande `date` existe dans Unix depuis les premiers jours. POSIX a standardisé la commande, donc elle fonctionne assez uniformément sur les systèmes Unix-like. `date` utilise généralement la timezone du système, mais tu peux la modifier avec `TZ`.

Autres méthodes :
- `hwclock` pour l'heure matérielle
- `date -u` pour UTC
- Les scripts en Perl ou Python pour des besoins complexes

Détails d'implémentation :
- `date` appelle des fonctions C standard comme `time()` et `strftime()` pour obtenir et formater la date.

## See Also
("Voir aussi")

- Man page: Tape `man date` dans le terminal pour lire le manuel.
- GNU Coreutils: [https://www.gnu.org/software/coreutils/manual/html_node/date-invocation.html](https://www.gnu.org/software/coreutils/manual/html_node/date-invocation.html)
- POSIX specifications: [https://pubs.opengroup.org/onlinepubs/9699919799/utilities/date.html](https://pubs.opengroup.org/onlinepubs/9699919799/utilities/date.html)
