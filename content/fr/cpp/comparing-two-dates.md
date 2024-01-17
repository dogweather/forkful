---
title:                "Comparer deux dates"
html_title:           "C++: Comparer deux dates"
simple_title:         "Comparer deux dates"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/cpp/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Qu'est-ce que & Pourquoi?
Comparer deux dates est une tâche commune pour les programmeurs en C++. Cela implique de comparer deux valeurs de date pour déterminer si elles sont égales, antérieures ou postérieures l'une à l'autre. Cette comparaison est généralement utilisée pour trier des données ou pour effectuer des calculs basés sur des dates.

## Comment faire:
Voici un exemple simple de comparaison de deux dates en C++:

```C++
#include <iostream>
#include <ctime>

int main(){
  std::time_t date1 = std::time(nullptr);
  std::time_t date2 = std::time(nullptr);

  if(date1 == date2){
    std::cout << "Les deux dates sont égales." << std::endl;
  } else if(date1 < date2){
    std::cout << "La première date est antérieure à la deuxième date." << std::endl;
  } else{
    std::cout << "La première date est postérieure à la deuxième date." << std::endl;
  }

  return 0;
}
```

Voici un exemple d'output basé sur la date actuelle :

```
La première date est postérieure à la deuxième date.
```

## Zoom en profondeur:
La comparaison de dates est devenue plus simple avec l'introduction du type de données de date standard en C++11. Avant cela, les programmeurs devaient utiliser les fonctions de manipulation de temps de la bibliothèque C pour effectuer des comparaisons de date. Il existe également des alternatives telles que l'utilisation de structures de données spécifiques à la date ou le stockage des dates en tant que chaînes de caractères.

## Voir aussi:
- [Documentation sur le type de données de date en C++](https://en.cppreference.com/w/cpp/chrono)
- [Guide pour utiliser les fonctions de manipulation de temps en C](https://www.programiz.com/c-programming/c-timing-clocks)
- [Utilisation de structures de données spécifiques à la date en C++](https://www.geeksforgeeks.org/c-program-find-difference-two-dates/)