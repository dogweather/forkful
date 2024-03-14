---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:13:40.806012-07:00
description: "Analyser une date \xE0 partir d'une cha\xEEne de caract\xE8res implique\
  \ d'interpr\xE9ter le format de la cha\xEEne pour extraire les composants de la\
  \ date tels que le\u2026"
lastmod: '2024-03-13T22:44:58.173153-06:00'
model: gpt-4-0125-preview
summary: "Analyser une date \xE0 partir d'une cha\xEEne de caract\xE8res implique\
  \ d'interpr\xE9ter le format de la cha\xEEne pour extraire les composants de la\
  \ date tels que le\u2026"
title: "Analyser une date depuis une cha\xEEne de caract\xE8res"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?
Analyser une date à partir d'une chaîne de caractères implique d'interpréter le format de la chaîne pour extraire les composants de la date tels que le jour, le mois et l'année. Les programmeurs font cela pour gérer les entrées utilisateur, lire des fichiers de données ou interagir avec des API qui communiquent les dates dans des formats de chaînes de caractères. C'est essentiel pour le traitement des données, la validation et l'exécution d'arithmétique sur les dates dans les applications.

## Comment faire :
Dans le C++ moderne, vous pouvez utiliser la bibliothèque `<chrono>` pour manipuler les dates et les heures de manière native, mais cela ne prend pas directement en charge l'analyse à partir de chaînes sans analyse manuelle pour des formats plus complexes. Cependant, pour les formats de date ISO 8601 et les formats personnalisés simples, voici comment vous pouvez accomplir l'analyse.

**Utilisation de `<chrono>` et `<sstream>` :**
```cpp
#include <iostream>
#include <sstream>
#include <chrono>
#include <iomanip>

int main() {
    std::string date_str = "2023-04-15"; // Format ISO 8601
    std::istringstream iss(date_str);
    
    std::chrono::year_month_day parsed_date;
    iss >> std::chrono::parse("%F", parsed_date);
    
    if (!iss.fail()) {
        std::cout << "Date analysée : " << parsed_date << std::endl;
    } else {
        std::cout << "Échec de l'analyse de la date." << std::endl;
    }
    
    return 0;
}
```
Exemple de sortie :
```
Date analysée : 2023-04-15
```

Pour des formats plus complexes ou lors de l'utilisation de versions antérieures de C++, des bibliothèques tierces comme `date.h` (la bibliothèque de dates de Howard Hinnant) sont populaires. Voici comment vous pouvez analyser différents formats avec celle-ci :

**Utilisation de la bibliothèque `date.h` :**
Assurez-vous d'avoir installé la bibliothèque. Vous pouvez la trouver [ici](https://github.com/HowardHinnant/date).

```cpp
#include "date/date.h"
#include <iostream>

int main() {
    std::string date_str = "Avril 15, 2023";
    
    std::istringstream iss(date_str);
    date::sys_days parsed_date;
    iss >> date::parse("%B %d, %Y", parsed_date);
    
    if (!iss.fail()) {
        std::cout << "Date analysée : " << parsed_date << std::endl;
    } else {
        std::cout << "Échec de l'analyse de la date à partir de la chaîne." << std::endl;
    }

    return 0;
}
```
Exemple de sortie (peut varier en fonction de la locale et des paramètres de date de votre système) :
```
Date analysée : 2023-04-15
```
