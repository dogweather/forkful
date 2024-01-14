---
title:                "C++: Obtenir la date actuelle"
programming_language: "C++"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/cpp/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Pourquoi

Saviez-vous qu'il est possible d'obtenir la date actuelle en C++ ? Que ce soit pour programmer un système de rappel, afficher la date dans votre jeu vidéo ou simplement pour votre propre curiosité, connaître la date actuelle est un outil utile pour tout programmeur. Dans cet article, nous verrons comment obtenir la date actuelle en C++ et plongerons plus en profondeur dans ce sujet fascinant.

## Comment faire

Pour obtenir la date actuelle en C++, nous utiliserons la bibliothèque standard <ctime>. Voici un exemple de code pour afficher la date actuelle :

```C++
#include <iostream>
#include <ctime>

int main() {
    // Obtient l'heure actuelle en utilisant la bibliothèque <ctime>
    std::time_t dateCourante = std::time(0);

    // Convertit l'heure en format lisible
    std::cout << "La date actuelle est : " << std::ctime(&dateCourante);
    
    return 0;
}
```
### Explication :

Dans cet exemple, nous utilisons la fonction <time> pour obtenir la date actuelle en utilisant la variable "dateCourante". Nous convertissons ensuite cette date en un format plus lisible en utilisant la fonction <ctime> et l'affichons à l'aide de la fonction "cout" de la bibliothèque <iostream>.

Voici un exemple de sortie pour ce code :

```
La date actuelle est : Thu Jun 03 16:07:19 2021
```

## Plongée en profondeur

Maintenant que nous savons comment obtenir la date actuelle en C++, voyons en quoi consiste réellement cette date. En utilisant la bibliothèque <ctime>, nous pouvons également accéder à des fonctions plus précises pour obtenir l'heure, les minutes et les secondes. Nous pouvons également utiliser des fonctions pour comparer des dates, les convertir en différents fuseaux horaires et bien plus encore.

Une autre chose intéressante à noter est que nous pouvons également modifier la date actuelle en utilisant la fonction <mktime>. Cela peut être utile pour créer un calendrier personnalisé ou pour tester des fonctionnalités basées sur la date dans un programme.

## Voir aussi

- <a href="https://www.cplusplus.com/reference/ctime/" target="_blank">Documentation de la bibliothèque <ctime></a>
- <a href="https://www.tutorialspoint.com/cplusplus/cpp_date_time.htm" target="_blank">Guide sur la manipulation de la date et de l'heure en C++</a>
- <a href="http://www.cplusplus.com/reference/ctime/mktime/" target="_blank">Fonction <mktime> de la bibliothèque <ctime></a>