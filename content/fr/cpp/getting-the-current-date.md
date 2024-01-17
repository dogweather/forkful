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

## Qu'est-ce que c'est et pourquoi le faire?

Obtenir la date actuelle est un élément important de la programmation en C ++. Cela permet aux programmeurs de connaître l'heure et la date exactes, ce qui peut être utile pour suivre le temps écoulé depuis le démarrage du programme ou pour afficher la date dans des messages ou des journaux.

## Comment faire:

Pour obtenir la date actuelle en C ++, vous pouvez utiliser la fonction "time" de la bibliothèque standard de C ++. Voici un exemple de code:

```C++
//include la bibliothèque de temps
#include <ctime>

//déclare une variable pour stocker la date et l'heure actuelles
time_t now = time(0);

//convertit la date et l'heure en une chaîne de caractères lisible par les humains
char* dt = ctime(&now);

//affiche la date et l'heure actuelles
cout << "La date et l'heure actuelles sont: " << dt << endl;
```

Voici un exemple de sortie:

```
La date et l'heure actuelles sont: Ven Mai 28 15:46:58 2021
```

## Plongée en profondeur:

Historiquement, la fonction "time" utilise un nombre de secondes écoulées depuis le 1er janvier 1970 pour représenter la date et l'heure. Cependant, elle peut également utiliser d'autres représentations en fonction du système d'exploitation.

Il existe également d'autres moyens d'obtenir la date et l'heure actuelles en C ++, tels que la bibliothèque boost et la bibliothèque chrono. Cependant, la fonction "time" reste la méthode la plus courante.

## Voir aussi:

- [Documentation de la fonction "time" en C ++](https://www.cplusplus.com/reference/ctime/time/)
- [Documentation de la bibliothèque boost](https://www.boost.org/doc/libs/1_76_0/doc/html/date_time.html)
- [Documentation de la bibliothèque chrono en C ++](https://en.cppreference.com/w/cpp/chrono)