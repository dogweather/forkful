---
title:                "Obtenir la date actuelle"
date:                  2024-01-20T15:12:54.672793-07:00
html_title:           "C: Obtenir la date actuelle"
simple_title:         "Obtenir la date actuelle"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c/getting-the-current-date.md"
---

{{< edit_this_page >}}

# Récupérer la date actuelle en C : Simple et pratique

## Quoi et Pourquoi ?
La récupération de la date actuelle c'est juste demander à votre ordi 'Quel jour on est?'. Les développeurs font ça pour des logs, des timestamps, des fonctionnalités basées sur la date... bref, plein de raisons.

## Comment faire :
Voilà le code. Simple, direct.

```c
#include <stdio.h>
#include <time.h>

int main() {
    time_t now = time(NULL);
    struct tm *local = localtime(&now);

    printf("Date: %02d/%02d/%04d\n", local->tm_mday, local->tm_mon + 1, local->tm_year + 1900);
    return 0;
}
```

Si on exécute, ça donne :

```
Date: 22/03/2023
```

## Plongée profonde
Historiquement, `time.h` est là depuis les premiers jours du C. On a aussi `gettimeofday` et `clock_gettime` pour plus de précision. La struct `tm` stocke des infos détaillées, et faire `local->tm_year + 1900` c'est parce que `tm_year` compte depuis 1900. Oui, c'est un peu vintage.

## Voir également :
- [Documentation de la librairie C - time.h](https://en.cppreference.com/w/c/chrono)
- [Man page de localtime(3)](https://linux.die.net/man/3/localtime)
- [Tutoriel sur les Dates et Heures en C](https://www.tutorialspoint.com/c_standard_library/c_function_localtime.htm)
