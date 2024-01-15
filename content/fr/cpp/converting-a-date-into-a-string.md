---
title:                "Convertir une date en chaîne de caractères"
html_title:           "C++: Convertir une date en chaîne de caractères"
simple_title:         "Convertir une date en chaîne de caractères"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/cpp/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Pourquoi
Si vous travaillez avec des dates dans votre projet C++, il est probable que vous ayez besoin de les convertir en chaînes de caractères à un moment donné. Cela peut être utile pour afficher une date dans un format spécifique ou pour la stocker dans une base de données. Dans cet article, nous allons explorer comment effectuer cette conversion en utilisant le langage C++.

## Comment faire
Tout d'abord, nous avons besoin d'une variable de type `std::tm` qui représente une date. Dans cet exemple, nous utiliserons la date actuelle en utilisant la fonction `std::time` :

```
#include <iostream>
#include <iomanip>
#include <ctime>

int main()
{
  std::time_t t = std::time(nullptr);
  std::tm* now = std::localtime(&t);
}
```

Maintenant que nous avons notre date, nous pouvons utiliser la fonction `std::put_time` pour la convertir en une chaîne de caractères. Cette fonction prend un format et une date en paramètres et renvoie une chaîne de caractères correspondant à la date dans le format spécifié. Par exemple, pour obtenir une date au format "dd/mm/yyyy" :

```
#include <iostream>
#include <iomanip>
#include <ctime>

int main()
{
  std::time_t t = std::time(nullptr);
  std::tm* now = std::localtime(&t);
  
  std::cout << std::put_time(now, "%d/%m/%Y") << std::endl;
}
```

Cela affichera la date actuelle dans le format spécifié. Vous pouvez également personnaliser le format en utilisant des spécificateurs de conversion de style `printf` dans le premier paramètre de `std::put_time`. Par exemple, pour afficher la date au format "hh:mm:ss" avec l'heure au format 12 heures :

```
#include <iostream>
#include <iomanip>
#include <ctime>

int main()
{
  std::time_t t = std::time(nullptr);
  std::tm* now = std::localtime(&t);
  
  std::cout << std::put_time(now, "%I:%M:%S %p") << std::endl;
}
```

Cela affichera l'heure actuelle dans le format spécifié avec "AM" ou "PM" à la fin pour indiquer la période de la journée.

## Plongée en profondeur
Si vous souhaitez utiliser des formats de date différents de ceux disponibles avec `std::put_time`, vous pouvez utiliser la fonction `std::strftime` qui prend également un format et une date en paramètres, mais qui offre une plus grande flexibilité. Vous pouvez trouver la liste complète des spécificateurs de conversion de style `printf` dans la documentation de `std::strftime`.

De plus, n'oubliez pas que les fonctions `std::put_time` et `std::strftime` ne prennent pas en compte les paramètres régionaux (langue et pays) pour les formats de date. Si vous souhaitez utiliser des noms de jours et de mois dans votre langue maternelle, vous devrez les fournir manuellement en utilisant des tableaux de chaînes.

## Voir aussi
- [Documentation de std::put_time](https://en.cppreference.com/w/cpp/io/manip/put_time)
- [Documentation de std::strftime](https://en.cppreference.com/w/cpp/chrono/c/strftime)