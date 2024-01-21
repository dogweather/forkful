---
title:                "Comparer deux dates"
date:                  2024-01-20T17:32:37.622055-07:00
model:                 gpt-4-1106-preview
simple_title:         "Comparer deux dates"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?
Comparer deux dates, c'est déterminer leur relation chronologique. Les programmeurs le font pour vérifier des échéances, trier des événements ou gérer des durées.

## Comment faire :
```C
#include <stdio.h>
#include <time.h>

int compare_dates(struct tm date1, struct tm date2) {
    double diff = difftime(mktime(&date1), mktime(&date2));
    if (diff > 0) {
        return 1; // date1 est après date2
    } else if (diff < 0) {
        return -1; // date1 est avant date2
    } else {
        return 0; // les dates sont identiques
    }
}

int main() {
    struct tm date1 = {.tm_year = 122, .tm_mon = 6, .tm_mday=15}; // 15 Juillet 2022
    struct tm date2 = {.tm_year = 122, .tm_mon = 8, .tm_mday=10}; // 10 Septembre 2022
    int result = compare_dates(date1, date2);
    if (result == 1) {
        printf("La première date est postérieure.\n");
    } else if (result == -1) {
        printf("La première date est antérieure.\n");
    } else {
        printf("Les deux dates sont identiques.\n");
    }
    return 0;
}
```
Sample output:
```
La première date est antérieure.
```

## Examen approfondi
Comparer des dates nous plonge dans la gestion du temps en programmation, avec ses zones de complexité comme les fuseaux horaires et les seconds intercalaires. Historiquement, des structures comme `struct tm` en C simplifient ce processus, mais elles ne sont pas sans pièges—n'oubliez pas que les années sont comptées à partir de 1900 et les mois à partir de 0. Alternatives ? La librairie `date.h` dans C++ ou des libs externes en C comme `libdaterange`. Pour l'implémentation, `difftime()` est standard et assez basique mais attentif aux détails comme l'heure d'été.

## Voir aussi
- [Manuel de référence C pour difftime](https://en.cppreference.com/w/c/chrono/difftime)
- [Tutoriel sur struct tm](https://www.tutorialspoint.com/c_standard_library/c_function_strftime.htm)
- [C time.h Library Functions](https://www.tutorialspoint.com/c_standard_library/time_h.htm)