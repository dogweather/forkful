---
title:                "C++: Comparer deux dates"
programming_language: "C++"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/cpp/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Pourquoi

Comme programmeur en C++, il est parfois nécessaire de comparer deux dates pour effectuer des opérations telles que la planification d'événements ou la gestion de tâches. Comprendre comment comparer des dates est une compétence importante à maîtriser pour tout développeur de logiciels.

## Comment faire

Pour comparer deux dates en C++, nous devons utiliser la classe "std::chrono::time_point". Cette classe représente un point spécifique dans le temps, et nous pouvons l'utiliser pour créer des instances de date et les comparer entre elles.

Voici un exemple de code montrant comment définir deux dates et les comparer entre elles :

```C++
// Importer les bibliothèques nécessaires
#include <iostream>
#include <chrono>
using namespace std; 

int main() {
    // Définir les dates à comparer
    chrono::time_point<chrono::system_clock> date1 = chrono::system_clock::now();
    chrono::time_point<chrono::system_clock> date2 = date1 + chrono::hours(24);

    // Comparer les dates
    if (date1 < date2) {
        cout << "Date 1 est plus tôt que date 2" << endl;
    } else if (date1 > date2) {
        cout << "Date 1 est plus tard que date 2" << endl;
    } else {
        cout << "Les dates sont égales" << endl;
    }

    return 0;
}
```

Voici un exemple de sortie pour illustrer la comparaison entre deux dates :

```
Date 1 est plus tôt que date 2
```

## Plongée en profondeur

Il est important de comprendre que les dates sont des objets complexes en C++, et qu'il existe plusieurs façons de les représenter et de les comparer. Par exemple, il existe différents types de "clocks" (horloges) en C++, et chacune peut fournir différents niveaux de précision pour les dates.

Il est également important de noter que les dates peuvent être affectées par des valeurs de timezone (fuseaux horaires) et des changements de temps, ce qui peut compliquer la comparaison entre elles. C'est pourquoi il est recommandé d'utiliser la classe "std::chrono::time_point" qui est conçue pour gérer ces détails et nous permettre de comparer les dates de manière précise.

## Voir aussi

- [Documentation sur classe "std::chrono::time_point" (en anglais)](https://en.cppreference.com/w/cpp/chrono/time_point)
- [Tutoriel sur les opérations de temps en C++ (en anglais)](https://www.geeksforgeeks.org/operations-date-time-c/)
- [Article sur la manipulation des dates et heures en C++ (en français)](https://blog.frankel.ch/manipulation-temps-date-heure-cpp/)