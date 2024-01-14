---
title:    "C: Calculer une date dans le futur ou le passé"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/c/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Pourquoi

Calculer une date dans le futur ou le passé est une compétence importante en programmation, car cela peut être nécessaire dans de nombreux projets. Par exemple, il peut être utile de savoir quelle journée de la semaine tombera un événement spécifique dans quelques mois.

## Comment faire

Pour calculer une date dans le futur ou le passé en langage C, il est important de comprendre la structure de la date et du temps. En C, la structure de la date et du temps est définie dans la bibliothèque <time.h>. Cette bibliothèque contient des fonctions utiles pour manipuler les dates et les heures.

Pour calculer une date dans le futur ou le passé, nous devons d'abord initialiser une valeur de type “time_t” avec la date actuelle. Cela peut être fait en utilisant la fonction time() qui renvoie le nombre de secondes depuis le 1er janvier 1970. Nous allons utiliser cette valeur pour ajouter ou soustraire des secondes, des minutes, des heures, des jours, des mois ou des années à la date actuelle.

Voici un exemple de code pour calculer une date dans le futur et afficher le résultat:

```C
#include <stdio.h>
#include <time.h>

int main()
{
    // Initialiser une valeur de type time_t avec la date actuelle
    time_t now = time(NULL);

    // Ajouter 60 secondes à la date actuelle
    now = now + 60;

    // Convertir la date en une structure de temps
    struct tm *timeinfo = localtime(&now);

    // Afficher la date dans un format spécifique
    printf("La date dans 1 minute sera: %s\n", asctime(timeinfo));

    return 0;
}
```

Output:

```
La date dans 1 minute sera: Wed Jul 1 22:45:13 2020
```

## Plongée en profondeur

En utilisant la structure de la date et du temps, nous pouvons manipuler différents composants tels que les secondes, les minutes, les heures, les jours, les mois et les années. Voici une liste des fonctions les plus couramment utilisées pour traiter les dates et les heures en C:

- localtime(): Permet de convertir une valeur de type time_t en une structure de temps locale.
- mktime(): Permet de convertir une structure de temps en une valeur de type time_t.
- asctime(): Permet de convertir une structure de temps en une représentation de chaîne de caractères lisible.
- strftime(): Permet de formater une structure de temps en une chaîne de caractères personnalisée.
- difftime(): Permet de calculer la différence en secondes entre deux valeurs de type time_t.

Il est important de comprendre que le calcul des dates dans le futur ou le passé peut être plus complexe que simplement ajouter ou soustraire une certaine quantité de temps. Les différences de temps peuvent varier selon les fuseaux horaires, les années bissextiles, etc. Il est donc crucial de bien comprendre les différentes fonctions disponibles et de bien gérer les erreurs possibles lors du traitement des dates et des heures.

## Voir aussi

- [Tutorialspoint - Manipulation de la date et de l'heure en C](https://www.tutorialspoint.com/c_standard_library/time_h.htm)
- [GeeksforGeeks - Afficher les dates et les heures en C](https://www.geeksforgeeks.org/c-program-print-current-day-date-time/)
- [Documentation officielle de C - Bibliothèque <time.h>](https://www.tutorialspoint.com/c_standard_library/time_h.htm)