---
title:                "C++: Convertir une date en chaîne de caractères"
programming_language: "C++"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/cpp/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Pourquoi
Si vous travaillez avec des dates dans votre code C ++, vous avez probablement déjà rencontré le besoin de convertir une date en une chaîne de caractères. Cela peut être utile pour afficher la date dans un format spécifique, la stocker dans une base de données ou l'utiliser dans une URL.

## Comment faire
La conversion d'une date en une chaîne de caractères peut sembler compliquée, mais heureusement, il existe des bibliothèques et des fonctions intégrées en C ++ pour faciliter cette tâche. Voici un exemple de code pour convertir une date en chaîne de caractères en utilisant la bibliothèque "chrono" et la fonction "strftime" :

```C++
#include <iostream>
#include <chrono>
#include <iomanip>
#include <ctime>

int main() {
  std::chrono::system_clock::time_point now = std::chrono::system_clock::now();
  std::time_t now_time = std::chrono::system_clock::to_time_t(now);

  // La chaîne de caractères sera écrite dans "date_string"
  char date_string[50];

  // Utilisation de la fonction strftime pour formater la date en une chaîne de caractères
  std::strftime(date_string, 50, "%d/%m/%Y", std::localtime(&now_time));

  std::cout << "La date d'aujourd'hui est : " << date_string << std::endl;
  return 0;
}
```

La sortie de ce code serait : "La date d'aujourd'hui est : 17/09/2021". Vous pouvez utiliser différents formats de date en modifiant la chaîne de format dans la fonction strftime. Consultez la documentation de strftime pour voir toutes les options disponibles.

## Plongée en profondeur
La bibliothèque "chrono" utilise le type de données "time_point" pour représenter une date en C ++. La fonction "to_time_t" convertit ce type de données en un type de données plus courant, "time_t". Ensuite, la fonction "strftime" utilise ce type de données pour formater la date en une chaîne de caractères.

Vous pouvez également utiliser la bibliothèque "boost" pour convertir une date en chaîne de caractères en C ++. Elle offre plus d'options de formatage pour la date et peut être utile si vous avez des dates provenant de différents fuseaux horaires.

## Voir aussi
- [Documentation de la fonction "strftime"](https://www.cplusplus.com/reference/ctime/strftime/)
- [Documentation de la bibliothèque "chrono"](https://en.cppreference.com/w/cpp/chrono)
- [Documentation de la bibliothèque "boost"](https://www.boost.org/doc/libs/1_63_0/doc/html/date_time.html)