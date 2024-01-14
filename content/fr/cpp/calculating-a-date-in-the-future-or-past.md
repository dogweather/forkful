---
title:    "C++: Calcul d'une date dans le futur ou dans le passé"
keywords: ["C++"]
---

{{< edit_this_page >}}

## Pourquoi

Il y a de nombreuses raisons pour lesquelles on pourrait vouloir calculer une date dans le futur ou le passé en utilisant un programme en C++. Peut-être avez-vous besoin de planifier un événement ou une tâche, ou peut-être êtes-vous simplement curieux de savoir quel jour de la semaine tombera une date spécifique dans quelques années.

## Comment Faire

La première étape pour calculer une date dans le futur ou le passé est de choisir une date de référence. Pour cet exemple, nous allons utiliser la date d'aujourd'hui et l'afficher en utilisant la bibliothèque de dates et d'heures de la norme C++.

```C++
#include <iostream>
#include <ctime>

int main()
{
  // Obtenir la date actuelle et l'assigner à une variable tm
  time_t now = time(0);
  tm *date_actuelle = localtime(&now);
  
  // Afficher la date en utilisant strftime()
  char date_string[11];
  strftime(date_string, 11, "%d/%m/%Y", date_actuelle);
  std::cout << "Date actuelle: " << date_string << std::endl;
  
  return 0;
}
```

Maintenant que nous avons la date actuelle, nous pouvons utiliser les fonctions de la bibliothèque de dates et d'heures pour calculer une date dans le futur ou le passé en ajoutant ou en soustrayant un nombre de jours.

```C++
// Calculer une date dans le futur en ajoutant 10 jours
tm date_future = *date_actuelle;
date_future.tm_mday += 10;
mktime(&date_future); // mise à jour de date_future pour qu'elle soit correctement formatée

// Afficher la date dans le futur en utilisant à nouveau strftime()
char date_future_string[11];
strftime(date_future_string, 11, "%d/%m/%Y", &date_future);
std::cout << "Dans 10 jours, nous serons le " << date_future_string << std::endl;

// Calculer une date dans le passé en soustrayant 5 jours
tm date_past = *date_actuelle;
date_past.tm_mday -= 5;
mktime(&date_past);

// Afficher la date dans le passé
char date_past_string[11];
strftime(date_past_string, 11, "%d/%m/%Y", &date_past);
std::cout << "Il y a 5 jours, nous étions le " << date_past_string << std::endl;
```

## Approfondissement

Le calcul de dates dans le futur ou le passé peut sembler simple, mais il existe en réalité de nombreuses complications à prendre en compte. Par exemple, comment gérer les années bissextiles ou les différents calendriers utilisés dans différents pays ? Il est également important de connaître les limites de la bibliothèque de dates et d'heures de la norme C++, en particulier en ce qui concerne les dates antérieures à 1900.

Si vous êtes intéressé par une approche plus avancée pour calculer des dates, vous pouvez également vous renseigner sur la librairie "Boost Date Time" qui offre une fonctionnalité plus complète pour travailler avec des dates et des heures en C++.

## Voir Aussi

- [Documentation de la bibliothèque de dates et d'heures de la norme C++](https://devdocs.io/cpp/header/chrono) 
- [Documentation de la librairie "Boost Date Time"](https://www.boost.org/doc/libs/1_61_0/doc/html/date_time.html)