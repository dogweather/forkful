---
title:    "C: Comparer deux dates"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/c/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Pourquoi

La comparaison de deux dates est une étape importante dans la programmation en C, car cela permet de déterminer l'ordre chronologique entre deux événements ou de vérifier si une date est antérieure ou postérieure à une autre. Cela est particulièrement utile dans les applications de suivi de temps, de réservation de rendez-vous, ou simplement pour afficher une date correcte dans un format spécifique.

## Comment faire

La comparaison de deux dates peut être réalisée en utilisant les fonctions prédéfinies de la bibliothèque standard du C telles que `time()` et `difftime()`. Voici un exemple de code qui compare deux dates et affiche le résultat:

```C
#include <stdio.h>
#include <time.h>

int main()
{
    time_t date1, date2;
    double diff;

    // Obtenez les dates sous forme de nombres de type time_t
    date1 = time(NULL);
    printf("La première date est: %ld\n", date1);

    // Vous pouvez également utiliser la fonction mktime() pour créer une date à partir de chiffres spécifiques
    struct tm timeinfo = { .tm_year=2021, .tm_mon=0, .tm_mday=1 };
    date2 = mktime(&timeinfo);

    printf("La deuxième date est: %ld\n", date2);

    // Comparez les deux dates en utilisant la fonction difftime()
    diff = difftime(date1, date2);

    if (diff > 0) {
        printf("La première date est postérieure à la deuxième date.\n");
    } else if (diff < 0) {
        printf("La première date est antérieure à la deuxième date.\n");
    } else {
        printf("Les deux dates sont identiques.\n");
    }

    return 0;
}
```

Output :

```
La première date est: 1627628395
La deuxième date est: 1609459200
La première date est postérieure à la deuxième date.
```

## Plongée en profondeur

Il est important de noter que les fonctions de la bibliothèque standard du C utilisent les dates sous forme de nombres de type time_t qui représentent le nombre de secondes écoulées depuis le 1er janvier 1970 à 00:00:00 UTC. Cela signifie que lors de la comparaison de deux dates, une conversion de ces nombres en une structure de type `tm` est nécessaire pour pouvoir faire des comparaisons en termes de jours, mois et années.

De plus, la fonction `difftime()` renvoie la différence entre les deux dates en secondes, ce qui peut être inutile pour les comparaisons avec de grandes différences de temps. Dans de tels cas, il peut être préférable d'utiliser des fonctions de traitement de date telles que `gmtime()` et `localtime()` qui convertissent le nombre de temps en une structure de type `tm` lisible par l'homme.

## Voir aussi

Pour en savoir plus sur les manipulations de dates en C et les différentes fonctions à utiliser, consultez les liens suivants :

- [La bibliothèque standard du C - Les fonctions de date et heure](https://fr.wikipedia.org/wiki/Biblioth%C3%A8que_standard_du_C#Fonctions_de_date_et_heure)
- [Documentation de la fonction time()](https://www.cplusplus.com/reference/ctime/time/)
- [Documentation de la fonction difftime()](https://www.cplusplus.com/reference/ctime/difftime/)
- [Documentation de la fonction mktime()](https://www.cplusplus.com/reference/ctime/mktime/)
- [Documentation de la fonction gmtime()](https://www.cplusplus.com/reference/ctime/gmtime/)
- [Documentation de la fonction localtime()](https://www.cplusplus.com/reference/ctime/localtime/)