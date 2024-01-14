---
title:                "C++: Calculer une date dans le futur ou le passé"
simple_title:         "Calculer une date dans le futur ou le passé"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/cpp/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Pourquoi

Il y a de nombreuses raisons pour lesquelles on souhaiterait calculer une date dans le futur ou dans le passé en programmation C++. Peut-être que vous travaillez sur une application de calendrier ou une fonctionnalité de rappel, ou peut-être que vous construisez un programme pour gérer des rendez-vous ou des échéances. Quelle que soit la raison, il peut être utile de savoir comment réaliser cette tâche en programmation.

## Comment faire

La méthode la plus simple pour calculer une date dans le futur ou dans le passé en C++ est d'utiliser la structure de données `std::tm` et la fonction `std::mktime()`. Voici un exemple de code montrant comment calculer une date dans 7 jours à partir d'aujourd'hui :

```C++
#include <iostream>
#include <iomanip>
#include <ctime>

int main()
{
    // Obtenir la date et l'heure actuelles
    std::time_t now = std::time(nullptr);
    // Convertir en structure tm
    std::tm *crtm = std::localtime(&now);
    // Ajouter 7 jours à la date actuelle
    crtm->tm_mday += 7;
    // Convertir la structure tm en temps
    std::time_t future = std::mktime(crtm);
    // Afficher la date dans un format plus lisible
    std::cout << "La date dans 7 jours sera : " << std::put_time(crtm, "%m/%d/%Y") << std::endl;
    return 0;
}
```

Output :

```
La date dans 7 jours sera : 08/26/2021
```

Il est important de noter que le calcul de la date dans le passé fonctionne de la même manière, il suffit de soustraire le nombre de jours souhaité de la date actuelle au lieu de l'ajouter.

## Plongée en profondeur

Bien qu'utiliser la structure de données `std::tm` et la fonction `std::mktime()` soit la méthode la plus simple pour calculer une date dans le futur ou dans le passé, il existe d'autres solutions possibles. Par exemple, vous pouvez également utiliser la bibliothèque `boost::date_time` ou la bibliothèque de dates C++11 pour effectuer ces calculs. Ces bibliothèques offrent une syntaxe plus intuitive et des fonctionnalités supplémentaires, comme la prise en compte automatique des années bissextiles.

## Voir aussi

- [Documentation sur la structure `std::tm` et la fonction `std::mktime()`](https://www.cplusplus.com/reference/ctime/mktime/)
- [Bibliothèque boost::date_time](https://www.boost.org/doc/libs/1_77_0/doc/html/date_time.html)
- [Bibliothèque de dates C++11](https://en.cppreference.com/w/cpp/chrono)