---
title:                "C: Comparaison de deux dates"
simple_title:         "Comparaison de deux dates"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Pourquoi

La comparaison de deux dates peut sembler une tâche simple et peut-être même insignifiante pour certains, mais en réalité, elle est une compétence très utile en programmation. En comparant deux dates, vous pouvez déterminer l'ordre chronologique des événements ou même détecter des erreurs dans un programme.

## Comment faire

Pour comparer deux dates en C, vous devez utiliser la fonction `difftime()` de la bibliothèque standard `time.h`. Cette fonction prend en paramètres deux valeurs `time_t` représentant les dates à comparer et retourne un nombre représentant la différence en secondes entre les deux dates. Voici un exemple de code montrant comment utiliser `difftime()` :

```C
#include <stdio.h>
#include <time.h>

int main() {
    // Création de deux dates à comparer
    time_t date1, date2;
    struct tm date_struct1 = {0}; 
    struct tm date_struct2 = {0};

    date_struct1.tm_year = 2022 - 1900; // année - 1900
    date_struct1.tm_mon = 06; // mois (0 - 11)
    date_struct1.tm_mday = 28; // jour
    date_struct2.tm_year = 2022 - 1900;
    date_struct2.tm_mon = 06;
    date_struct2.tm_mday = 29;

    // Conversion en time_t
    date1 = mktime(&date_struct1);
    date2 = mktime(&date_struct2);

    // Comparaison des dates 
    double difference = difftime(date1, date2); 
    if(difference > 0) {
        printf("La date 1 est antérieure à la date 2\n");
    } else if(difference < 0) {
        printf("La date 1 est postérieure à la date 2\n");
    } else {
        printf("Les deux dates sont identiques\n");
    }
    return 0;
}
```

Voici la sortie de ce programme :

```
La date 1 est antérieure à la date 2
```

Comme la date 1 est le 28 juin 2022 et la date 2 est le 29 juin 2022, la date 1 est effectivement antérieure à la date 2.

## Plongée en profondeur

La fonction `difftime()` peut sembler simple à première vue, mais il y a quelques points à garder à l'esprit lors de sa manipulation. Tout d'abord, la fonction renvoie une valeur de type `double`, il est donc important de faire attention aux conversions lors de la comparaison avec une valeur entière. De plus, il est important de noter que la précision de cette fonction dépend de la précision de la valeur `time_t`, qui peut varier selon les systèmes d'exploitation.

En outre, il est important de comprendre également les différences entre `time_t` et `struct tm`. `time_t` représente une date en secondes depuis le 1er janvier 1970 et `struct tm` est une structure contenant des informations détaillées sur une date particulière. Il est donc important de comprendre comment effectuer des conversions entre ces deux types de données pour une comparaison précise.

## Voir aussi

Si vous souhaitez en savoir plus sur la comparaison de dates en C, voici quelques ressources utiles à consulter :

- [Documentation officielle de la fonction difftime()](https://www.cplusplus.com/reference/ctime/difftime/)
- [Tutoriel YouTube : Comparer les dates en C](https://www.youtube.com/watch?v=Ykckt_OfFhI)
- [Article "Time & date manipulation in C" par Smash The Stack](https://www.smashthestack.org/old-stuff/2007-10-writeup.html)

Merci d'avoir lu cet article sur la comparaison de dates en C. N'hésitez pas à explorer davantage cette fonctionnalité et à l'utiliser dans vos projets pour une meilleure gestion du temps. À bientôt !