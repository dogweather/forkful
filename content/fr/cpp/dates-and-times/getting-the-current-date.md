---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:09:07.117094-07:00
description: "La r\xE9cup\xE9ration de la date actuelle en C++ est une t\xE2che fondamentale\
  \ pour les programmes qui doivent traiter ou afficher des dates bas\xE9es sur l'horloge\u2026"
lastmod: '2024-02-25T18:49:54.834490-07:00'
model: gpt-4-0125-preview
summary: "La r\xE9cup\xE9ration de la date actuelle en C++ est une t\xE2che fondamentale\
  \ pour les programmes qui doivent traiter ou afficher des dates bas\xE9es sur l'horloge\u2026"
title: Obtenir la date actuelle
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?
La récupération de la date actuelle en C++ est une tâche fondamentale pour les programmes qui doivent traiter ou afficher des dates basées sur l'horloge du système. C'est essentiel pour la journalisation, la datation, la planification de tâches et toute fonctionnalité qui repose sur des dates et des heures.

## Comment faire :
C++ propose plusieurs moyens d'obtenir la date actuelle, y compris la bibliothèque standard C++ et des bibliothèques tierces comme Boost. Les exemples suivants démontrent comment accomplir cette tâche.

### En utilisant `<chrono>` (C++20 et ultérieur)
C++20 a introduit plus de fonctionnalités dans la bibliothèque `<chrono>`, ce qui simplifie l'obtention de la date actuelle :
```cpp
#include <iostream>
#include <chrono>
#include <format> // Pour std::format (C++20)

int main() {
    auto current_time_point = std::chrono::system_clock::now(); // Capture le moment actuel
    auto current_time_t = std::chrono::system_clock::to_time_t(current_time_point); // Convertir en time_t

    // Formater l'heure dans un format lisible
    std::cout << "Date Actuelle : " << std::format("{:%Y-%m-%d}", std::chrono::system_clock::to_time_t(current_time_point)) << std::endl;

    return 0;
}
```
**Sortie d'exemple :**
```plaintext
Date Actuelle : 2023-03-15
```

### En utilisant `<ctime>`
Pour les programmeurs travaillant avec des versions antérieures de C++ ou ceux qui préfèrent la bibliothèque C traditionnelle :
```cpp
#include <iostream>
#include <ctime>

int main() {
    std::time_t t = std::time(0); // Obtenir l'heure actuelle
    std::tm* now = std::localtime(&t);
    std::cout << "Date Actuelle : " 
              << (now->tm_year + 1900) << '-' 
              << (now->tm_mon + 1) << '-'
              <<  now->tm_mday
              << std::endl;

    return 0;
}
```
**Sortie d'exemple :**
```plaintext
Date Actuelle : 2023-03-15
```

### En utilisant Boost Date_Time
Pour les projets qui utilisent les bibliothèques Boost, la bibliothèque Boost Date_Time offre une méthode alternative pour obtenir la date actuelle :
```cpp
#include <iostream>
#include <boost/date_time.hpp>

int main() {
    // Obtenir le jour actuel en utilisant le calendrier grégorien de Boost
    boost::gregorian::date today = boost::gregorian::day_clock::local_day();
    std::cout << "Date Actuelle : " << today << std::endl;

    return 0;
}
```
**Sortie d'exemple :**
```plaintext
Date Actuelle : 2023-Mar-15
```
Ces exemples offrent une base essentielle pour travailler avec des dates en C++, cruciale pour une large gamme d'applications.
