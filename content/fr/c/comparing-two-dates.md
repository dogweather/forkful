---
title:                "Comparaison de deux dates"
html_title:           "C: Comparaison de deux dates"
simple_title:         "Comparaison de deux dates"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est et pourquoi le faire?

Comparer deux dates en programmation est simplement le fait de vérifier si une date est chronologiquement antérieure ou postérieure à une autre. Les programmeurs le font souvent pour trier des événements chronologiquement ou pour vérifier la validité de données.

## Comment faire:

Voici deux façons simples de comparer deux dates en C:

```C
// Exemple 1: Comparaison à l'aide de la fonction strcmp()
#include <stdio.h>
#include <string.h>

int main()
{
    char date1[] = "12-05-2021";
    char date2[] = "06-05-2021";

    // Utilisation de la fonction strcmp() pour comparer les chaînes de caractères
    int result = strcmp(date1, date2);

    if(result > 0)
    {
        printf("%s est après %s\n", date1, date2);
    }
    else if(result < 0)
    {
        printf("%s est avant %s\n", date1, date2);
    }
    else
    {
        printf("Les dates sont les mêmes\n");
    }
    
    return 0;
}

// Exemple 2: Comparaison à l'aide de variables de type struct tm

#include <stdio.h>
#include <time.h>

int main()
{
    struct tm date1 = {0};
    date1.tm_mday = 12;
    date1.tm_mon = 4;
    date1.tm_year = 121;

    struct tm date2 = {0};
    date2.tm_mday = 6;
    date2.tm_mon = 4;
    date2.tm_year = 121;

    // Utilisation de la fonction difftime() pour calculer la différence entre les deux dates
    double difference = difftime(mktime(&date1), mktime(&date2));

    if(difference > 0)
    {
        printf("%d-%d-%d est après %d-%d-%d\n", date1.tm_mday, date1.tm_mon, date1.tm_year, date2.tm_mday, date2.tm_mon, date2.tm_year);
    }
    else if(difference < 0)
    {
        printf("%d-%d-%d est avant %d-%d-%d\n", date1.tm_mday, date1.tm_mon, date1.tm_year, date2.tm_mday, date2.tm_mon, date2.tm_year);
    }
    else
    {
        printf("Les dates sont les mêmes\n");
    }

    return 0;
}
```

La sortie pour les deux exemples sera:
```
12-05-2021 est après 06-05-2021
```

## Plongée en profondeur:

En plus des méthodes présentées ci-dessus, il existe également d'autres moyens de comparer deux dates en C. Avant que la fonction `difftime()` soit introduite, les programmeurs utilisaient souvent la fonction `mktime()` pour convertir les dates en un format plus facilement comparable. Il est également important de noter que la comparaison de deux dates peut être différente en fonction de la précision requise. Par exemple, pour comparer des dates dans une application financière, il peut être nécessaire de prendre en compte les heures et les minutes, tandis que pour comparer des dates dans une application de planification, seule la date peut être importante.

## Voir aussi:

Vous pouvez en apprendre plus sur la comparaison de dates en C en consultant les sources suivantes:

- [La référence C](https://www.cplusplus.com/reference/ctime/)
- [Le tutoriel sur les structures et les dates en C](https://www.tutorialspoint.com/cprogramming/c_date_time.htm)