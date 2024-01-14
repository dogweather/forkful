---
title:    "C: Calculer une date dans le futur ou le passé"
keywords: ["C"]
---

{{< edit_this_page >}}

## Pourquoi

Si vous travaillez sur une application ou un programme qui nécessite de connaître une date dans le futur ou le passé, il est important de savoir comment calculer ces dates. Heureusement, il existe des fonctions en langage C qui peuvent vous aider à le faire facilement.

## Comment faire

La fonction principale à utiliser pour calculer une date dans le futur ou le passé est `mktime()`. Cette fonction prend en compte une structure `tm` qui représente une date sous la forme d'années, de mois, de jours, d'heures, de minutes et de secondes.

```C
#include <stdio.h>
#include <time.h>

int main() {
    // Définition d'une structure tm pour représenter la date actuelle
    struct tm current_date = {
        .tm_sec = 0,
        .tm_min = 0,
        .tm_hour = 0,
        .tm_mday = 3,
        .tm_mon = 9,    // Octobre (0-11)
        .tm_year = 121, // 2021 - 1900
    };
    
    // Calcul de la date dans 3 jours en utilisant mktime
    current_date.tm_mday += 3;
    mktime(&current_date);
    
    // Affichage de la nouvelle date
    printf("La date dans 3 jours sera: %d/%d/%d", current_date.tm_mday, current_date.tm_mon + 1, current_date.tm_year + 1900);
    
    return 0;
}
```

Output: `La date dans 3 jours sera: 6/10/2021`

Nous pouvons également utiliser `mktime()` pour calculer une date dans le passé. Dans ce cas, il suffit de soustraire le nombre de jours souhaité de la structure `tm` avant d'appeler la fonction `mktime()`. Voici un exemple:

```C
// Soustraction de 5 jours à la date actuelle
current_date.tm_mday -= 5;
mktime(&current_date);

// Affichage de la nouvelle date
printf("La date il y a 5 jours était: %d/%d/%d", current_date.tm_mday, current_date.tm_mon + 1, current_date.tm_year + 1900);
```

Output: `La date il y a 5 jours était: 28/9/2021`

## Plongée en profondeur

Il est important de noter que la fonction `mktime()` ne prend en compte que les dates comprises entre 1970 et 2038. Cela est dû à une limitation du type de données utilisé pour stocker l'heure dans ces fonctions.

Cependant, si vous avez besoin de travailler avec des dates en dehors de cette plage, vous pouvez utiliser la fonction `timegm()` qui prend en compte les années après 2038.

De plus, la fonction `mktime()` peut également retourner des valeurs de date et d'heure incorrectes si elles sont en dehors de la plage autorisée dans le type de données. Il est donc important de bien vérifier les valeurs retournées avant de les utiliser dans votre code.

## Voir aussi

- [Documentation officielle de la fonction mktime()](https://www.cplusplus.com/reference/ctime/mktime/)
- [Exemples de code pour calculer des dates avec mktime()](https://www.includehelp.com/c-programs/date-time-programming-in-c-with-mktime-function.aspx)