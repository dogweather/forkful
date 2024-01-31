---
title:                "Analyse d'une date à partir d'une chaîne de caractères"
date:                  2024-01-20T15:35:04.498023-07:00
simple_title:         "Analyse d'une date à partir d'une chaîne de caractères"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/cpp/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?
Parsez une date dans C++, c'est transformer un texte (comme "01/04/2023") en données manipulables par le programme. On fait ça pour traiter ou comparer des dates, vital en planification ou archivage.

## Comment faire :
```c++
#include <iostream>
#include <sstream>
#include <chrono>
#include <iomanip>

int main() {
    std::string date_str = "01/04/2023"; // format DD/MM/YYYY
    std::tm tm = {};
    std::istringstream ss(date_str);
    ss >> std::get_time(&tm, "%d/%m/%Y"); // Modifiez le format ici si nécessaire.

    if (ss.fail()) {
        std::cerr << "Erreur de parsing de la date." << std::endl;
        return 1;
    }

    // Conversion en time_point pour une utilisation facile avec <chrono>
    std::chrono::system_clock::time_point tp = std::chrono::system_clock::from_time_t(std::mktime(&tm));
    
    // Affichage pour confirmer le résultat :
    std::cout << "Année: " << tm.tm_year + 1900 << ", Mois: " << tm.tm_mon + 1 << ", Jour: " << tm.tm_mday << std::endl;
    return 0;
}

```
Sortie attendue :
```
Année: 2023, Mois: 4, Jour: 1
```

## Plongeon profond :
Historiquement, C++ a utilisé la bibliothèque C pour la manipulation de dates, ce qui n'était pas toujours idéal. `std::get_time` et C++ <chrono> sont plus récents. Ils offrent précision et facilité. Alternativement, on peut utiliser `strptime` sur *nix ou `std::from_chars` depuis C++17 pour plus de performance. La gestion des timezones est complexe; pour cela, il y a des bibliothèques tierces comme Howard Hinnant's date library.

## Voir aussi :
- Documentation de `<chrono>`: https://en.cppreference.com/w/cpp/header/chrono
- Howard Hinnant's Date library: https://github.com/HowardHinnant/date
- Tutoriel sur `std::get_time`: https://en.cppreference.com/w/cpp/io/manip/get_time
- Fonctions alternatives `strptime` : https://linux.die.net/man/3/strptime
- Documentation `std::from_chars`: https://en.cppreference.com/w/cpp/utility/from_chars
