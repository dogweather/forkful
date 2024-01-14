---
title:    "C: Comparer deux dates"
keywords: ["C"]
---

{{< edit_this_page >}}

## Pourquoi

Comparer deux dates peut sembler être une tâche simple en apparence, mais c'est en réalité un sujet complexe pour les programmeurs. Cela peut sembler être une petite fonctionnalité, mais elle est souvent utilisée dans de nombreux programmes. La compréhension de la comparaison de dates en C peut donc être extrêmement utile pour améliorer vos compétences en programmation et vous aider à devenir un meilleur développeur.

## Comment faire

Pour comparer deux dates en C, nous allons utiliser la fonction `difftime`, qui est définie dans la bibliothèque standard `time.h`. Cette fonction renvoie la différence entre deux valeurs de temps en secondes. Pour cela, nous devrons d'abord convertir les dates en structures de données de type `time_t` à l'aide de la fonction `mktime`. Voici un exemple de code pour comparer deux dates dans le format "jour/mois/année" :

```
#include <stdio.h>
#include <time.h>

int main() {
    // Première date à comparer
    struct tm date1 = { .tm_mday = 1, .tm_mon = 0, .tm_year = 2020 - 1900 };
    // Seconde date à comparer
    struct tm date2 = { .tm_mday = 1, .tm_mon = 0, .tm_year = 2019 - 1900 };

    // Convertir les dates en valeurs de temps
    time_t time1 = mktime(&date1);
    time_t time2 = mktime(&date2);

    // Comparer les dates et stocker le résultat dans une variable de type double
    double diff = difftime(time1, time2);

    if (diff > 0) {
        printf("Date 1 est plus récente que Date 2\n");
    } else if (diff < 0) {
        printf("Date 2 est plus récente que Date 1\n");
    } else {
        printf("Les deux dates sont identiques\n");
    }
    
    return 0;
}

// Sortie: Date 1 est plus récente que Date 2
```

Dans cet exemple, nous utilisons la fonction `mktime` pour convertir les dates entrées en `struct tm`, puis nous comparons les valeurs de temps en utilisant la fonction `difftime`. La sortie variera en fonction des dates que vous entrez.

## Plongée en profondeur

La comparaison de dates en C peut sembler simple, mais il y a plusieurs choses à prendre en compte lors de sa mise en œuvre. Tout d'abord, les dates doivent être valides et correctement formatées pour pouvoir être converties en `struct tm`. De plus, la fonction `difftime` renvoie une valeur de type `double`, il est donc important de manipuler correctement cette valeur en utilisant des conditions pour déterminer si la première date est antérieure, égale ou postérieure à la seconde. Enfin, il existe d'autres méthodes pour comparer des dates en C, telles que la fonction `strftime`, qui permet de formater et afficher des dates de manière plus précise.

## Voir aussi

- [Documentation officielle sur la fonction `difftime` en C](https://www.cplusplus.com/reference/ctime/difftime/)
- [Guide sur les dates et heures en C](https://www.tutorialspoint.com/c_standard_library/time_h.htm)
- [Autres méthodes pour comparer des dates en C](https://www.geeksforgeeks.org/compare-two-dates-c/)