---
title:                "Comparer deux dates"
html_title:           "Clojure: Comparer deux dates"
simple_title:         "Comparer deux dates"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?
Comparer deux dates signifie déterminer quelle date est antérieure, postérieure ou si elles sont identiques. La comparaison des dates est essentielle pour la gestion des tâches ou événements dans les applications de programmation.

## Comment faire :
Voici un exemple simple de comparaison de deux dates en utilisant la structure tm de la bibliothèque time.h en C:
```C
#include <stdio.h>
#include <time.h>

int main(void) {
    struct tm date1 = {0, 0, 12, 25, 10, 120}; // 12:00, 25 Novembre 2020
    struct tm date2 = {0, 5, 12, 25, 10, 120}; // 12:05, 25 Novembre 2020

    time_t t1 = mktime(&date1);
    time_t t2 = mktime(&date2);

    if(t1 < t2) printf("La date1 est antérieure à date2\n");
    else if(t1 > t2) printf("La date1 est postérieure à date2\n");
    else printf("La date1 est identique à date2\n");

    return 0;
}
```
L'exécution de ce programme affiche : "La date1 est antérieure à date2".

## Approfondissement :
Comparer deux dates peut parfois être plus complexe que notre exemple simple. Vous devez tenir compte des fuseaux horaires, des secondes intercalaires et d'autres subtiles.

Dans l'histoire de la programmation, il existe de nombreux cas où une mauvaise comparaison de dates a provoqué des erreurs graves, par exemple, le célèbre bug de l'an 2000.

Une alternative à l'utilisation de la bibliothèque time.h pourrait être d'écrire votre propre fonction de comparaison de dates, mais cela pourrait conduire à des erreurs et des cas limites difficiles à gérer.

Le code précédent compare simplement deux valeurs de type time_t en utilisant les opérateurs de comparaison standards de C.

## Voir Aussi :
Pour des informations supplémentaires sur la comparaison des dates en C ou d'autres fonctions de date et d'heure, consultez les liens suivants:

- Documentation de la bibliothèque time.h : 
  [C library to handle date and time (time.h)](https://www.cplusplus.com/reference/ctime/)

- Tutoriels et exemples de programmes C : 
  [C programming examples](https://www.programiz.com/c-programming/examples) 

- Comparaison des dates et heures dans différentes langues de programmation : 
  [Comparing Dates and Times Across Programming Languages](https://dzone.com/articles/comparing-dates-and-times-across-programming-langu)