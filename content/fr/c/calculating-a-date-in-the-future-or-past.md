---
title:                "Calcul d'une date dans le futur ou le passé"
html_title:           "C: Calcul d'une date dans le futur ou le passé"
simple_title:         "Calcul d'une date dans le futur ou le passé"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous êtes un programmeur débutant ou avancé en C, vous savez probablement que la manipulation des dates peut être un défi. Mais pourquoi devriez-vous vous intéresser à calculer une date dans le futur ou le passé? Eh bien, tout simplement parce que cela peut être très utile dans de nombreuses applications, telles que la réservation de vols, la planification d'événements ou la gestion de tâches.

## Comment faire

Pour calculer une date dans le futur ou le passé en utilisant le langage de programmation C, il existe plusieurs méthodes à votre disposition. Voici deux exemples de code pour ajouter ou soustraire des jours à une date donnée en utilisant des fonctions intégrées dans la bibliothèque time.h:

```
#include <stdio.h>
#include <time.h>

int main()
{
    // Calculer la date dans 15 jours
    time_t now = time(NULL); // Obtient l'heure actuelle
    struct tm* tm_date = localtime(&now); // Convertit l'heure actuelle en structure de date
    tm_date->tm_mday += 15; // Ajoute 15 jours

    // Afficher la nouvelle date
    char buffer[80];
    strftime(buffer, sizeof(buffer), "%A, %B %d, %Y", tm_date);
    printf("Dans 15 jours, nous serons un %s\n", buffer);

    // Calculer la date il y a 1 mois
    time_t now = time(NULL); // Obtient l'heure actuelle
    struct tm* tm_date = localtime(&now); // Convertit l'heure actuelle en structure de date
    tm_date->tm_mon -= 1; // Soustrait 1 mois

    // Afficher la nouvelle date
    char buffer[80];
    strftime(buffer, sizeof(buffer), "%A, %B %d, %Y", tm_date);
    printf("Il y a 1 mois, nous étions un %s\n", buffer);

    return 0;
}
```

Ces exemples sont basés sur la structure de données "tm" qui représente une date en C. Les champs de cette structure peuvent être modifiés pour calculer une date ultérieure ou antérieure en ajoutant ou soustrayant des jours, mois ou années.

## Plongée en profondeur

Lorsque vous calculez une date dans le futur ou le passé en utilisant C, il est important de prendre en compte des éléments tels que les années bissextiles et les fuseaux horaires. Cela peut sembler compliqué, mais en comprenant la structure "tm" et les fonctions de manipulation de temps disponibles dans la bibliothèque time.h, vous devriez être en mesure de gérer ces cas spéciaux.

## Voir aussi

Pour en savoir plus sur la manipulation de dates en C, vous pouvez consulter les liens suivants:

- [Documentation officielle de la bibliothèque time.h](https://www.gnu.org/software/libc/manual/html_node/Time-Manipulation.html)
- [Tutoriel sur la manipulation de dates en C](https://www.tutorialspoint.com/c_standard_library/c_function_strftime.htm)
- [Forum de discussion sur la manipulation de dates en C](https://www.codeproject.com/Questions/870365/C-time-localtime-struct-tm-Problem)