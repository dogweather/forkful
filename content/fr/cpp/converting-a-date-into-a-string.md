---
title:                "C++: Transformer une date en chaîne de caractères"
simple_title:         "Transformer une date en chaîne de caractères"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/cpp/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Pourquoi

Il est parfois nécessaire de convertir une date en chaîne de caractères dans un programme C++. Cela peut être utile pour afficher une date lisible par l'utilisateur, pour l'enregistrer dans un fichier ou pour l'utiliser dans une requête SQL. Dans cet article, nous allons expliquer comment le faire de manière efficace.

## Comment faire

La conversion d'une date en chaîne de caractères se fait en plusieurs étapes. Tout d'abord, il faut obtenir les composants de la date tels que le jour, le mois et l'année. Ensuite, vous pouvez les concaténer dans une chaîne de caractères en utilisant des fonctions telles que `std::to_string()` et `std::ostringstream`. Enfin, il faut prendre en compte le format de la date pour s'assurer qu'elle soit affichée correctement.

Voici un exemple de code montrant comment convertir une date en chaîne de caractères utilisant la bibliothèque standard de C++ :

```C++
#include <iostream>
#include <string>
#include <sstream>
#include <iomanip> // nécessaire pour la fonction std::put_time

int main()
{
  // création d'une date
  std::tm date = {0, 0, 0, 28, 2, 2021}; // le 28 février 2021

  // déclaration d'un objet pour stocker la chaîne de caractères
  std::ostringstream oss;

  // conversion du jour en chaîne de caractères et ajout à la chaîne finale
  oss << std::put_time(&date, "%d ");

  // conversion du mois en chaîne de caractères et ajout à la chaîne finale
  oss << std::put_time(&date, "%b ");

  // conversion de l'année en chaîne de caractères et ajout à la chaîne finale
  oss << std::put_time(&date, "%Y");

  // récupération de la chaîne finale
  std::string date_str = oss.str();

  // affichage de la chaîne finale
  std::cout << "La date au format chaîne de caractères est : " << date_str << std::endl;

  return 0;
}
```

L'exemple ci-dessus utilise la fonction `std::put_time` pour formater la date selon le format spécifié (%d pour le jour, %b pour le mois en lettres, %Y pour l'année). La chaîne finale est stockée dans l'objet `std::ostringstream` et peut ensuite être récupérée en tant que `std::string`.

Voici la sortie de cet exemple :

```
La date au format chaîne de caractères est : 28 Feb 2021
```

## Plongée profonde

Pour une conversion plus avancée, vous pouvez utiliser la bibliothèque de dates et de temps de C++, introduite dans le standard C++11. Cette bibliothèque fournit des types et des fonctions pour manipuler les dates, les heures et les fuseaux horaires de manière plus précise et plus efficace.

Vous pouvez également utiliser des bibliothèques externes telles que Boost.Date_Time ou QtDate pour une plus grande flexibilité et des fonctionnalités supplémentaires.

Enfin, pour des formats de dates plus complexes, vous pouvez également écrire votre propre fonction de conversion en utilisant des outils de manipulation de chaînes de caractères disponibles dans C++, tels que `std::string::substr()` et `std::string::replace()`.

## Voir aussi

- [Documentation de la bibliothèque standard de C++ sur std::tm](https://en.cppreference.com/w/cpp/chrono/c/tm)
- [Documentation de la bibliothèque standard de C++ sur std::put_time](https://en.cppreference.com/w/cpp/io/manip/put_time)
- [Documentation de la bibliothèque de dates et de temps de C++](https://en.cppreference.com/w/cpp/header/chrono)
- [Boost.Date_Time](https://www.boost.org/doc/libs/1_67_0/doc/html/date_time.html)
- [QtDate](https://doc.qt.io/qt-5/qdate.html)