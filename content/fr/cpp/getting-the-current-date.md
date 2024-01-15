---
title:                "Obtenir la date actuelle"
html_title:           "C++: Obtenir la date actuelle"
simple_title:         "Obtenir la date actuelle"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/cpp/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous êtes un programmeur, il est fort probable que vous ayez besoin de connaître la date actuelle dans votre code. Cela peut être pour afficher la date dans une application, pour suivre la date de création d'un fichier ou pour effectuer des opérations basées sur la date. Dans cet article, nous allons voir comment obtenir la date actuelle en C++.

## Comment faire

Pour obtenir la date actuelle en C++, vous pouvez utiliser la fonction `std::chrono::system_clock::now()`. Cette fonction renvoie un objet `std::chrono::time_point` qui représente l'instant actuel. Ensuite, pour afficher la date, vous pouvez utiliser la fonction `std::chrono::system_clock::to_time_t()` pour convertir l'objet `time_point` en un objet `std::time_t`. Enfin, vous pouvez utiliser la fonction `std::localtime()` pour convertir l'objet `std::time_t` en une structure `std::tm` contenant toutes les informations sur la date et l'heure actuelles.

Voici un exemple de code qui montre comment obtenir la date actuelle et l'afficher dans un format personnalisé :

```C++
#include <iostream>
#include <ctime>
#include <iomanip>

int main()
{
    // Obtient l'instant actuel
    std::chrono::time_point now = std::chrono::system_clock::now();

    // Convertit en std::time_t
    std::time_t raw_time = std::chrono::system_clock::to_time_t(now);

    // Convertit en std::tm 
    std::tm* date = std::localtime(&raw_time);

    // Affiche la date sous un format personnalisé
    std::cout << "Date actuelle : "
              << std::put_time(date, "%A %d %B %Y") << std::endl;

    return 0;
}
```

Lorsque vous exécutez ce code, vous devriez obtenir un résultat similaire à celui-ci :

```
Date actuelle : mercredi 24 mars 2021
```

## Plongée en profondeur

La fonction `std::chrono::system_clock::now()` utilise une horloge système pour obtenir l'instant actuel. Cette horloge est basée sur un compteur qui s'incrémente à chaque seconde. Cependant, elle n'est pas garantie d'être précise à la seconde près et peut varier en fonction du système d'exploitation.

De plus, la fonction `std::localtime()` peut également être sensible aux paramètres régionaux du système et peut donc donner des résultats différents selon le système d'exploitation utilisé.

Si vous avez besoin d'une précision plus élevée ou d'une compatibilité avec différents systèmes d'exploitation, vous pouvez utiliser la bibliothèque `<chrono>` du C++11 qui offre des classes pour travailler avec le temps, comme `std::chrono::high_resolution_clock` et `std::chrono::steady_clock`.

## Voir aussi

- [std::chrono::system_clock](https://en.cppreference.com/w/cpp/chrono/system_clock)
- [std::chrono::time_point](https://en.cppreference.com/w/cpp/chrono/time_point)
- [std::localtime](https://en.cppreference.com/w/cpp/chrono/c/localtime)