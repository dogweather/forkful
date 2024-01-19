---
title:                "Comparer deux dates"
html_title:           "Clojure: Comparer deux dates"
simple_title:         "Comparer deux dates"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/cpp/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Quoi et Pourquoi?

Comparer deux dates consiste à évaluer quelle date est antérieure, ultérieure ou si elles sont identiques. Les programmeurs le font souvent lors de la manipulation de données chronologiques.

## Comment faire:

Voici un exemple de code en C++ utilisant la bibliothèque `<chrono>` pour comparer deux dates:

```C++
#include <iostream>
#include <chrono>

using namespace std;
using namespace chrono;

int main()
{
    // Crées deux dates de system_clock
    system_clock::time_point date1 = system_clock::now();
    system_clock::time_point date2 = system_clock::now() + hours(24);
  
    // Compareresaon 
    if (date1 == date2)
        cout << "Les dates sont identiques." << endl;
    else if (date1 < date2)
        cout << "Date1 est antérieure à Date2." << endl;
    else
        cout << "Date1 est ultérieure à Date2." << endl;
  
    return 0;
}
```
Dans cet exemple, la date1 est toujours antérieure à la date2 car nous ajoutons 24 heures à la date2.

## Plongée profonde

* **Contexte historique** : Avant C++11, la comparaison de deux dates n'était pas une tâche simple. Il fallait travailler avec les dates en tant que chaînes de caractères ou comme un nombre d'unités de temps écoulées depuis une certaine date (généralement le 1er janvier 1970).
* **Alternatives** : Vous pouvez également utiliser <ctime> ou des bibliothèques tierces, comme Boost.DateTime. Cependant, elles peuvent être plus verbeuses ou nécessiter des dépendances supplémentaires.
* **Détails d'implémentation** : `<chrono>` fournit trois types de "clocks", et chacune peut être utilisée pour marquer l'heure actuelle de manière différente.

## Voir aussi

* Documentation officielle sur C++ std::chrono: https://en.cppreference.com/w/cpp/chrono
* Article sur la compréhension des dates et heures en C++: https://www.learncpp.com/cpp-tutorial/89-class-code-and-applications/
* Tutoriel sur l'utilisation de Boost.DateTime: https://theboostcpplibraries.com/boost.datetime-time