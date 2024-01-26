---
title:                "Obtenir la date actuelle"
date:                  2024-01-20T15:13:15.485885-07:00
html_title:           "C: Obtenir la date actuelle"
simple_title:         "Obtenir la date actuelle"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/cpp/getting-the-current-date.md"
---

{{< edit_this_page >}}

## What & Why? - Quoi et Pourquoi?
En programmation, obtenir la date actuelle signifie lire la date et l'heure du système sur lequel le programme s'exécute. Les programmeurs le font pour timestamping, des fonctionnalités liées au calendrier, et pour suivre les événements en temps réel.

## How to: - Comment faire :
```C++
#include <iostream>
#include <chrono>
#include <ctime>

int main() {
    // Utilisation de <chrono> pour la date/heure actuelle
    auto now = std::chrono::system_clock::now();
    std::time_t end_time = std::chrono::system_clock::to_time_t(now);

    // Conversion en format de date/heure local
    std::tm *ptm = std::localtime(&end_time);
    char buffer[32];
    // Formatage de la date/heure sous la forme yyyy-mm-dd hh:mm:ss
    std::strftime(buffer, 32, "%Y-%m-%d %H:%M:%S", ptm);

    std::cout << "Date et heure actuelles: " << buffer << std::endl;

    return 0;
}
```

Exemple de sortie :
```
Date et heure actuelles: 2023-04-05 14:07:31
```

## Deep Dive - Exploration approfondie
Avant C++11, `<ctime>` était généralement utilisé pour obtenir l'heure. Cependant, `<chrono>`, introduit avec C++11, offre une approche moderne pour manipuler le temps grâce à ses multiples fonctions et types prédéfinis. `<chrono>` est plus robuste, précis et flexible comparé à `<ctime>`.

Alternatives pour obtenir la date incluent l'utilisation de bibliothèques tierces comme Boost.DateTime. Ces bibliothèques peuvent offrir des fonctionnalités supplémentaires et une meilleure portabilité entre différents systèmes.

Les détails d'implémentation pour obtenir la date et l'heure actuelles dépendront du système d'exploitation sous-jacent puisque l'accès à l'horloge système est typiquement une fonctionnalité système native.

## See Also - Voir aussi
- [cppreference.com, Chrono](https://en.cppreference.com/w/cpp/chrono)
- [cppreference.com, Ctime](https://en.cppreference.com/w/cpp/header/ctime)
- [Boost.Date_Time](https://www.boost.org/doc/libs/1_75_0/doc/html/date_time.html)
