---
title:                "La comparaison de deux dates"
html_title:           "C: La comparaison de deux dates"
simple_title:         "La comparaison de deux dates"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Pourquoi
Si vous travaillez avec des dates dans vos programmes en C, vous aurez probablement besoin de les comparer à un moment ou un autre. Cela peut sembler simple, mais il y a quelques choses à savoir pour s'assurer que la comparaison se fait correctement. Dans cet article, nous allons explorer comment comparer deux dates en utilisant le langage C.

## Comment faire
Pour comparer deux dates en C, nous devrons utiliser la structure de données `tm` fournie par la bibliothèque standard de C. Tout d'abord, nous devons déclarer deux variables de type `tm` pour représenter nos dates :

```C
#include <stdio.h>
#include <time.h>

int main()
{
    struct tm date1, date2;
    // Code pour initialiser les dates ici
}
```

Ensuite, nous pouvons utiliser la fonction `mktime` pour convertir chaque date en une valeur numérique correspondant au nombre de secondes écoulées depuis le 1er Janvier 1970. Cette valeur sera plus facile à comparer que deux structures `tm` :

```C
// Code pour initialiser les dates ici
time_t seconds1 = mktime(&date1);
time_t seconds2 = mktime(&date2);
```

Maintenant, nous pouvons simplement utiliser les opérateurs de comparaison tels que `>, <, ==` pour comparer les deux valeurs numériques et déterminer quelle date est plus récente. Voici un exemple complet :

```C
#include <stdio.h>
#include <time.h>

int main()
{
    struct tm date1 = { .tm_year = 2021, .tm_mon = 7, .tm_mday = 1 };
    struct tm date2 = { .tm_year = 2021, .tm_mon = 10, .tm_mday = 15 };

    time_t seconds1 = mktime(&date1);
    time_t seconds2 = mktime(&date2);

    if (seconds1 > seconds2)
    {
        printf("La date 1 est plus récente que la date 2.");
    }
    else
    {
        printf("La date 2 est plus récente que la date 1.");
    }

    return 0;
}
```

Et voici la sortie de cet exemple :

```
La date 2 est plus récente que la date 1.
```

## Plongée en profondeur
Il est important de noter que la fonction `mktime` utilise un fuseau horaire local pour convertir la structure `tm` en une valeur de temps. Cela peut entraîner des problèmes de précision si vous devez comparer des dates dans des fuseaux horaires différents.

De plus, si vous devez comparer des dates avec une précision supérieure à la journée, vous devrez utiliser d'autres fonctions comme `difftime` qui permet d'obtenir un résultat en secondes avec une précision d'une seconde.

## Voir aussi
- [Documentation de la bibliothèque standard C sur la structure `tm`](https://en.cppreference.com/w/c/chrono/tm)
- [Documentation de la fonction `mktime`](https://en.cppreference.com/w/c/chrono/mktime)
- [Documentation de la fonction `difftime`](https://en.cppreference.com/w/c/chrono/difftime)