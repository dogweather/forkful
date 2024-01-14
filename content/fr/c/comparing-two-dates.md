---
title:                "C: Comparaison de deux dates"
programming_language: "C"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c/comparing-two-dates.md"
---

{{< edit_this_page >}}

# Pourquoi

Comparer les dates est une tâche courante en programmation, que vous travailliez sur un projet personnel ou professionnel. Cela permet de déterminer si une date est antérieure, postérieure ou égale à une autre, et peut être utile pour la gestion de données ou la mise en œuvre de fonctionnalités spécifiques.

# Comment faire

Pour comparer deux dates en C, nous utilisons les fonctions de la bibliothèque standard "time.h". Tout d'abord, nous devons convertir les dates en deux variables de type "struct tm", qui stockent les informations de date, telles que l'année, le mois, le jour, etc.

```C 
struct tm date1, date2;
```

Ensuite, nous pouvons utiliser la fonction "mktime()" pour convertir ces structures en valeurs de type "time_t", qui représentent le nombre de secondes écoulées depuis le 1er janvier 1970.

```C 
time_t converted_date1 = mktime(&date1);
time_t converted_date2 = mktime(&date2);
```

À partir de là, nous pouvons simplement utiliser des opérateurs de comparaison tels que "==", "<", ">" pour comparer les dates converties.

```C 
if (converted_date1 == converted_date2) {
    printf("Les deux dates sont égales.");
} else if (converted_date1 < converted_date2) {
    printf("La date 1 est antérieure à la date 2.");
} else {
    printf("La date 2 est antérieure à la date 1.");
}
```

Voici un exemple de sortie pour la date 1 = 1er janvier 2020 et la date 2 = 1er février 2020 :

```
La date 1 est antérieure à la date 2.
```

# Approfondissement

Pour une comparaison plus précise, nous pouvons également utiliser la fonction "difftime()" pour calculer la différence en secondes entre les deux dates. Cela peut être utile pour déterminer la différence entre deux événements, par exemple.

```C 
double difference = difftime(converted_date2, converted_date1);
```

Nous pouvons également utiliser les fonctions de la bibliothèque "localtime" pour travailler avec les dates locales ou "gmtime" pour les dates en temps universel (GMT).

# Voir aussi

- [Documentation de la bibliothèque "time.h"](https://www.gnu.org/software/libc/manual/html_node/Date-and-Time.html)
- [Tutoriel sur le traitement des dates en C](https://www.programiz.com/c-programming/c-date-time-functions)
- [Exemple de programme comparant deux dates en C](https://www.tutorialspoint.com/compare-two-dates-in-c-language)