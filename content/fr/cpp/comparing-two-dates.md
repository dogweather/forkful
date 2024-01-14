---
title:    "C++: Comparer deux dates"
keywords: ["C++"]
---

{{< edit_this_page >}}

## Pourquoi

L'une des tâches les plus courantes en programmation est de comparer deux dates. Cela peut être nécessaire pour vérifier si une date est antérieure ou postérieure à une autre, ou pour effectuer des opérations de tri. Dans ce billet de blog, nous allons explorer comment comparer deux dates en utilisant le langage de programmation C++. 

## Comment faire

Pour comparer deux dates en C++, nous allons utiliser la classe "std::chrono::time_point" et ses fonctions associées. Tout d'abord, nous allons déclarer deux variables de type time_point, qui représenteront nos deux dates à comparer. Ensuite, nous allons utiliser la fonction "std::chrono::time_point::operator<" pour vérifier si la première date est antérieure à la seconde. Voici un exemple de code :

```C++
// Déclaration des deux dates à comparer
std::chrono::time_point date1 = /* date */;
std::chrono::time_point date2 = /* autre date */;

// Vérification si date1 est antérieure à date2
if (date1 < date2) {
    std::cout << "La date1 est antérieure à la date2" << std::endl;
}
```

En utilisant le même principe, nous pouvons également vérifier si une date est postérieure à une autre en utilisant l'opérateur "std::chrono::time_point::operator>". Il est également possible de vérifier l'égalité entre deux dates en utilisant l'opérateur "std::chrono::time_point::operator==". Enfin, pour savoir si une date est incluse dans un intervalle donné, nous pouvons utiliser les opérateurs "std::chrono::time_point::operator>=" et "std::chrono::time_point::operator<=". Voici un exemple complet de code :

```C++
// Déclaration des deux dates à comparer
std::chrono::time_point date1 = /* date */;
std::chrono::time_point date2 = /* autre date */;

// Vérification si date1 est antérieure à date2
if (date1 < date2) {
    std::cout << "La date1 est antérieure à la date2" << std::endl;
}

// Vérification si date2 est postérieure à date1
if (date2 > date1) {
    std::cout << "La date2 est postérieure à la date1" << std::endl;
}

// Vérification si date1 est égale à date2
if (date1 == date2) {
    std::cout << "La date1 est égale à la date2" << std::endl;
}

// Vérification si date1 est incluse dans un intervalle de dates
std::chrono::time_point debut = /* date de début */ ;
std::chrono::time_point fin = /* date de fin */;
if (date1 >= debut && date1 <= fin) {
    std::cout << "La date1 est incluse dans l'intervalle de dates" << std::endl;
}
```

## Plongée en profondeur

Il est important de noter que la classe "std::chrono::time_point" représente une durée de temps écoulée depuis un point de référence, appelé "epoch". Le choix de l'epoch dépend de la plateforme et peut varier d'un système à un autre. De plus, la précision de la comparaison des dates peut également varier en fonction de l'epoch choisi. Il est donc important de se référer à la documentation de la plateforme utilisée pour connaître les détails de l'epoch et de sa précision.

## Voir aussi

- [Documentation officielle de C++ sur std::chrono](https://en.cppreference.com/w/cpp/chrono)
- [Tutorialspoint - Comparaison de dates en C++](https://www.tutorialspoint.com/how-to-compare-dates-in-cplusplus)