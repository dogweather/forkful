---
title:                "Calculer une date dans le futur ou le passé"
html_title:           "C++: Calculer une date dans le futur ou le passé"
simple_title:         "Calculer une date dans le futur ou le passé"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/cpp/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?

Calculer une date dans le futur ou dans le passé est une tâche fréquente en programmation qui permet de manipuler et de prédire le temps. C'est essentiel pour les planificateurs d'événements, les rappels, les logs système, et beaucoup plus d'applications.

## Comment faire :

Voici un exemple simple de calcul d'une date future en utilisant la bibliothèque `<chrono>` de C++.

```C++
#include <iostream>
#include <chrono>
#include <ctime>

int main() {
  using namespace std::chrono;

  // Obtenir l'heure actuelle
  system_clock::time_point now = system_clock::now();
  // Durée de deux semaines
  weeks span(2);

  // Calculer la date dans le futur
  system_clock::time_point future_date = now + span;

  // Convertir en time_t pour une meilleure visibilité
  time_t tt = system_clock::to_time_t(future_date);
  
  std::cout << "Dans deux semaines, on sera le : " << ctime(&tt);
  
  return 0;
}
```

Exemple de sortie : 

```
Dans deux semaines, on sera le : Thu Feb 11 12:37:23 2023
```

## Deep Dive 

Historiquement, les programmeurs utilisaient des fonctions intégrées comme `gmtime` ou `localtime` pour calculer les dates futures ou passées. Cependant, avec l'introduction de `<chrono>` dans C++11, une manière plus robuste et typiquement sûre de manipuler le temps est disponible.

Pour des calculs de date plus complexes ou si on travaille avec des fuseaux horaires, on peut regarder du côté de bibliothèques tiers comme [date](https://github.com/HowardHinnant/date) ou [libical](https://github.com/libical/libical).

L'implémentation derrière `<chrono>` est basée sur l'horloge système de l'ordinateur. Il est donc important de prendre en compte la possibilité d'un changement d'heure dû au passage à l'heure d'été ou d'hiver.

## Voir aussi :

1. [Documentation officielle de `<chrono>`](http://www.cplusplus.com/reference/chrono/)
2. [Bibliothèque "date" de Howard Hinnant](https://github.com/HowardHinnant/date)
3. [Bibliothèque libical](https://github.com/libical/libical)