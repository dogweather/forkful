---
title:    "C++: Comparer deux dates"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/cpp/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Pourquoi

Nous avons souvent besoin de comparer deux dates dans nos programmes C++, que ce soit pour vérifier les délais de livraison, les anniversaires ou simplement pour trier une liste de données. Dans cet article, nous allons découvrir comment comparer facilement deux dates en utilisant C++.

## Comment faire

La méthode la plus simple pour comparer deux dates en C++ est d'utiliser la structure de données `tm` de la bibliothèque `<ctime>`. Cette structure contient des informations sur le temps, y compris les éléments pour les années, les mois, les jours, les heures, les minutes et les secondes.

```C++
#include <iostream>
#include <ctime>

int main()
{
    // Déclaration des variables pour les deux dates à comparer
    struct tm date1 = {0, 0, 0, 1, 0, 2020}; // 1er janvier 2020
    struct tm date2 = {0, 0, 0, 15, 1, 2020}; // 15 février 2020

    // Comparaison des dates en utilisant la fonction difftime()
    double difference = difftime(mktime(&date1), mktime(&date2));

    // Affichage du résultat
    if (difference < 0) 
    {
        std::cout << "La date 1 est antérieure à la date 2." << std::endl;
    }
    else if (difference > 0)
    {
        std::cout << "La date 2 est antérieure à la date 1." << std::endl;
    }
    else 
    {
        std::cout << "Les deux dates sont identiques." << std::endl;
    }

    return 0;
}
```

Output:

```
La date 1 est antérieure à la date 2.
```

Dans cet exemple, nous avons utilisé la fonction `mktime()` pour convertir notre structure `tm` en un nombre de secondes depuis l'Epoch (1er janvier 1970). Ensuite, nous avons utilisé la fonction `difftime()` pour comparer ces deux nombres de secondes.

Il est également possible d'utiliser la fonction `mktime()` pour convertir une date saisie par l'utilisateur en une structure `tm`, ce qui vous permet de comparer facilement cette date avec une autre.

Il est important de noter que cette méthode ne prend pas en compte les fuseaux horaires et utilise l'heure locale par défaut. Si vous avez besoin de prendre en compte les fuseaux horaires ou des dates dans un fuseau horaire différent, vous devrez utiliser une bibliothèque externe telle que `Boost.Date_Time`.

## Plongée en profondeur

En utilisant la structure `tm`, il est également possible de comparer des dates avec une précision allant jusqu'aux millisecondes en utilisant les éléments `tm_sec` et `tm_usec` qui représentent respectivement les secondes et les microsecondes.

Dans certains cas, il peut également être utile de simplement comparer les dates sans tenir compte de l'heure ou des minutes. Pour ce faire, vous pouvez utiliser la fonction `std::equal()` de la bibliothèque `<algorithm>` en spécifiant les éléments de la structure `tm` que vous souhaitez comparer.

## Voir aussi

- Documentation sur la bibliothèque `<ctime>` en C++ : https://www.cplusplus.com/reference/ctime/
- Documentation sur la structure `tm` : https://www.cplusplus.com/reference/ctime/tm/
- Tutoriel sur la bibliothèque `Boost.Date_Time` : https://theboostcpplibraries.com/boost.datetime-date-time-convenience-classes