---
title:                "Calcul d'une date future ou passée"
date:                  2024-01-20T17:28:33.649013-07:00
model:                 gpt-4-1106-preview
html_title:           "C++: Calcul d'une date future ou passée"
simple_title:         "Calcul d'une date future ou passée"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/cpp/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Quoi et Pourquoi ?
Calculer une date dans le futur ou le passé, c'est changer une date de base d'un certain intervalle. On le fait pour des rappels, des échéances, des plannings, etc.

## Comment faire :
```C++
#include <iostream>
#include <ctime>

int main() {
    // La date d'aujourd'hui basée sur le système
    std::time_t t = std::time(nullptr);
    std::tm now = *std::localtime(&t);
    
    // Afficher la date d'aujourd'hui
    std::cout << "Aujourd'hui : " << now.tm_mday << "-" << (now.tm_mon + 1) << "-" << (now.tm_year + 1900) << '\n';
    
    // Calculons 30 jours dans le futur
    const int daysAhead = 30; // Nombre de jours à ajouter
    std::time_t future = t + (daysAhead * 24 * 60 * 60);  // 24 heures, 60 minutes, 60 secondes
    std::tm futureDate = *std::localtime(&future);
    
    // Afficher la date dans le futur
    std::cout << "Dans " << daysAhead << " jours : " << futureDate.tm_mday << "-" << (futureDate.tm_mon + 1) << "-" << (futureDate.tm_year + 1900) << '\n';

    return 0;
}
```
Sortie attendue (varie selon la date courante) :
```
Aujourd'hui : 26-3-2023
Dans 30 jours : 25-4-2023
```

## Détails :
Historiquement, le calcul de date était complexe et sujet à erreurs, notamment à cause des différents calendriers et des changements d'heures. Aujourd'hui, la bibliothèque standard de C++ simplifie ce processus grâce à la struct `std::tm`.

Des alternatives à `std::tm` incluent l'utilisation de bibliothèques tierces comme Boost.Date_Time ou la librairie `chrono` dans C++ moderne.

Pour la mise en œuvre, les éléments clés comprennent la gestion correcte des années bissextiles, des fuseaux horaires et des changements d'heure – l'utilisation de standards éprouvés est recommandée.

## Voir aussi :
- C++ Reference for `<ctime>`: https://en.cppreference.com/w/cpp/header/ctime
- Boost.Date_Time: https://www.boost.org/doc/libs/release/libs/date_time/
- C++ Chrono Library: https://en.cppreference.com/w/cpp/header/chrono
