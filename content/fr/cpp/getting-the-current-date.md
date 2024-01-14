---
title:                "C++: Obtenir la date actuelle"
simple_title:         "Obtenir la date actuelle"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/cpp/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Pourquoi

Obtenir la date et l'heure actuelles est une tâche courante en programmation. Cela peut être utile pour enregistrer la date et l'heure d'un événement, pour générer des rapports ou simplement pour afficher l'heure actuelle sur un programme.

## Comment faire

Il existe différentes façons d'obtenir la date et l'heure actuelles en C++. L'une des plus courantes est d'utiliser la fonction time() de la bibliothèque <ctime>. Cette fonction renvoie le nombre de secondes écoulées depuis le 1er janvier 1970, appelé le temps UNIX.

```C++
#include <iostream>
#include <ctime>

using namespace std;

int main() {
    // obtenir le temps actuel
    time_t now = time(0);
    
    // convertir le temps actuel en une structure tm
    tm *ltm = localtime(&now);
    
    // afficher la date et l'heure actuelles
    cout << "Date et heure actuelles: " << ltm->tm_mday << "/" 
        << ltm->tm_mon + 1 << "/" << ltm->tm_year + 1900 << " "
        << ltm->tm_hour << ":" << ltm->tm_min << ":" << ltm->tm_sec << endl;
        
    return 0;
}
```

La sortie sera quelque chose comme ceci : "Date et heure actuelles : 20/05/2021 14:30:00"

## Exploration approfondie

Pour le moment, nous avons seulement obtenu la date et l'heure actuelles sous forme de chaîne de caractères. Mais que se passe-t-il si nous voulons effectuer des calculs ou manipuler les éléments individuels de la date et de l'heure ?

Heureusement, la structure tm que nous avons utilisée dans l'exemple précédent contient toutes les informations nécessaires. Par exemple, ltm->tm_mday contient le jour du mois, ltm->tm_mon contient le mois et ainsi de suite. Vous pouvez utiliser ces informations pour effectuer des calculs ou pour créer un format de date personnalisé.

## Voir aussi

- [Documentation sur la fonction time()](https://www.cplusplus.com/reference/ctime/time/)
- [Exemples de codes pour obtenir la date et l'heure actuelles en C++](https://www.w3schools.com/cpp/cpp_date.asp)