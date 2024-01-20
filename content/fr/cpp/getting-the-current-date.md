---
title:                "Obtenir la date actuelle"
html_title:           "Bash: Obtenir la date actuelle"
simple_title:         "Obtenir la date actuelle"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/cpp/getting-the-current-date.md"
---

{{< edit_this_page >}}

# Date du jour en C++ : Pourquoi et Comment 

## Qu'est-ce et Pourquoi ?

Obtenir la date courante dans un programme C++ implique la récupération de l'information sur l'année, le mois et le jour exacts pendant l'exécution du programme. C'est essentiel pour enregistrer des événements, générer des horodatages et gérer les opérations dépendantes du temps.

## Comment faire:

C++ propose plusieurs façons d'obtenir la date et heure courantes. Voici un exemple courant utilisant `<chrono>` et `<iomanip>`:

```C++
#include <iostream>
#include <chrono>
#include <iomanip>

int main() {
    auto now = std::chrono::system_clock::now();
    std::time_t now_time = std::chrono::system_clock::to_time_t(now);
    std::cout << std::put_time(std::localtime(&now_time), "%F") << '\n';
    return 0;
}
```
Ce code affiche la date courante en format AAAA-MM-JJ. Exemple de sortie :
```
2023-07-01
``` 

## Plongée profonde

Historiquement, les développeurs C++ s'appuyaient sur `time.h` (ou `ctime` en C++) pour obtenir la date courante. Cependant, `<chrono>` est devenu le choix préféré depuis C++11 en raison de sa précision et de sa portabilité.

Comme alternative, on peut aussi utiliser `localtime_s` (pour Windows) ou `localtime_r` (pour Unix) pour une stratégie thread-safe, car `std::localtime` n'est pas thread-safe.

L'implémentation interne de l'obtention de la date courante dépend de l'API de l'OS sous-jacent. En général, celle-ci interagit avec l'horloge du système pour obtenir les informations actuelles de date et d'heure.

## Voir aussi :

- La documentation officielle de C++ pour [chrono](https://en.cppreference.com/w/cpp/chrono)
- Des informations supplémentaires sur [time.h](http://www.cplusplus.com/reference/ctime/)
- Un aperçu des [alongements de durée](https://en.cppreference.com/w/cpp/chrono) en C++