---
title:                "Analyse d'une date à partir d'une chaîne de caractères"
date:                  2024-01-20T15:34:46.858057-07:00
html_title:           "Arduino: Analyse d'une date à partir d'une chaîne de caractères"
simple_title:         "Analyse d'une date à partir d'une chaîne de caractères"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Quoi et Pourquoi ?
Parser une date depuis une chaîne de caractères, c'est lire et convertir cette chaîne pour obtenir une structure date utilisable en C. On le fait pour traiter les dates (comme des entrées utilisateur) de façon standardisée et manipulable.

## Comment faire :
```C
#include <stdio.h>
#include <time.h>

int main() {
    struct tm tm;
    char *date_str = "2023-04-01T15:42:00";
    if (strptime(date_str, "%Y-%m-%dT%H:%M:%S", &tm) != NULL) {
        printf("Année: %d, Mois: %d, Jour: %d, Heure: %d, Minute: %d, Seconde: %d\n",
               tm.tm_year + 1900, tm.tm_mon + 1, tm.tm_mday, tm.tm_hour, tm.tm_min, tm.tm_sec);
    } else {
        printf("Échec du parsing de la date.\n");
    }
    return 0;
}
```
Sortie:
```
Année: 2023, Mois: 4, Jour: 1, Heure: 15, Minute: 42, Seconde: 0
```

## Exploration détaillée
Historiquement, la conversion de chaînes de caractères en structures de date a toujours été une nécessité pour l'échange de données, en particulier avant l'ère d'Internet, quand la plupart des échanges étaient basés sur des formats textuels. En C, `strptime` est fonction standard pour parser les chaînes de caractères en dates. Il est accompagné par `strftime`, qui fait l'inverse, i.e., convertir une structure `tm` en chaîne de caractères.
Des alternatives incluent l'utilisation de bibliothèques tierces comme `getdate` qui est une partie de GNU et souvent utilisée dans les systèmes Unix. Le parsing personnalisé est également une option, mais nécessite une attention rigoureuse aux détails pour éviter les erreurs et les vulnérabilités, comme le débordement de tampon. En termes d’implémentation, `strptime` remplit une structure `tm` (temps) avec les composants décomposés de la date et de l'heure, permettant une manipulation facile et une interopérabilité avec les autres fonctions de temps standard de C telles que `mktime`.

## À voir également
- Documentation de `strptime` : [https://www.man7.org/linux/man-pages/man3/strptime.3.html](https://www.man7.org/linux/man-pages/man3/strptime.3.html)
- GNU `getdate` : [https://www.gnu.org/software/libc/manual/html_node/Low_002dLevel-Time-String-Parsing.html#Low_002dLevel-Time-String-Parsing](https://www.gnu.org/software/libc/manual/html_node/Low_002dLevel-Time-String-Parsing.html#Low_002dLevel-Time-String-Parsing)
- C Standard Library `<time.h>` : [https://en.cppreference.com/w/c/chrono](https://en.cppreference.com/w/c/chrono)
