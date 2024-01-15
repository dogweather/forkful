---
title:                "Calculer une date dans le futur ou le passé"
html_title:           "C++: Calculer une date dans le futur ou le passé"
simple_title:         "Calculer une date dans le futur ou le passé"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/cpp/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Pourquoi

Vous êtes peut-être plongé dans une nouvelle application de gestion de tâches ou vous prévoyez un voyage, et vous voulez savoir quelle sera la date dans un mois ou dans un an à partir de maintenant. Cela peut sembler contre-intuitif, mais calculer une date dans le futur ou dans le passé peut être très utile dans de nombreuses situations.

## Comment faire

Pour calculer une date dans le futur ou dans le passé en utilisant C++, vous devez d'abord inclure la bibliothèque `<chrono>` qui fournit des fonctions pour travailler avec le temps. Ensuite, vous devez créer un objet de type `std::chrono::system_clock::time_point`, qui contiendra la date actuelle. Ensuite, vous pouvez effectuer des calculs en utilisant les fonctions `std::chrono::duration` et `std::chrono::time_point`. Voici un exemple de code pour calculer et afficher la date dans 10 jours :

```C++
#include <chrono>
#include <iostream>

int main() {
    // Crée un objet time_point avec la date actuelle
    std::chrono::system_clock::time_point date = std::chrono::system_clock::now();

    // Calcule la date dans 10 jours en ajoutant une duration de 10 jours
    date += std::chrono::duration<int>(10);

    // Affiche la date dans le format YYYY/MM/DD
    std::cout << std::chrono::system_clock::to_time_t(date) << std::endl;

    return 0;
}
```

Le code ci-dessus utilise des fonctions de la bibliothèque `<chrono>` pour ajouter 10 jours à la date actuelle et afficher la nouvelle date au format [UNIX time](https://en.wikipedia.org/wiki/Unix_time). Vous pouvez également utiliser des fonctions telles que `std::chrono::duration_cast` pour convertir la date dans d'autres formats.

## Plongée profonde

Si vous voulez en savoir plus sur la manipulation des dates en C++, il y a quelques concepts clés à comprendre. Tout d'abord, il y a les différents types de time_point, qui peuvent être utilisés pour représenter le temps absolu, relatif ou en heures et minutes. Ensuite, il y a les durations, qui sont des unités de temps telles que les jours, les heures, les minutes, etc. En utilisant ces concepts, vous pouvez effectuer des calculs précis avec les dates et les heures en C++.

Pour une explication plus détaillée sur l'utilisation de `<chrono>` en C++, vous pouvez consulter la documentation officielle de [C++ référence](https://fr.cppreference.com/w/cpp/chrono).

## Voir aussi

- [Manipulation des dates et heures en C++](https://www.tutorialspoint.com/cplusplus/cpp_date_time.htm)
- [La bibliothèque <chrono> en C++](https://www.geeksforgeeks.org/time-point-structures-cpp/)
- [Calculer une date dans le futur ou dans le passé en C++](https://www.softwaretestinghelp.com/calculate-date-and-time-in-cpp/)