---
title:                "Calcul d'une date dans le futur ou le passé"
html_title:           "C: Calcul d'une date dans le futur ou le passé"
simple_title:         "Calcul d'une date dans le futur ou le passé"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi? 
Calculer une date dans le futur ou le passé est une tâche courante pour les programmeurs. Cela consiste à déterminer une date future ou passée en fonction d'une date initiale et d'un certain nombre de jours, mois ou années. Les programmeurs utilisent souvent cette fonction pour automatiser des tâches liées à la gestion du temps, telles que la planification d'événements ou la création d'alertes.

## Comment faire:
Voici un exemple de code en C qui calcule une date dans le futur en ajoutant 10 jours à la date actuelle :

```C
#include <stdio.h>
#include <time.h>

int main() {
  int jours = 10; // le nombre de jours à ajouter
  time_t todaysDate = time(NULL); // récupère la date actuelle sous forme de timestamp
  struct tm *futureDate = localtime(&todaysDate); // convertit le timestamp en une structure de date et heure
 
  // ajoute le nombre de jours à la date actuelle
  futureDate->tm_mday += days;
 
  // convertit la structure back en timestamp
  time_t futureDateTimestamp = mktime(futureDate);
 
  // affiche la date future en utilisant le format "DD/MM/YYYY"
  printf("La date future est: %02d/%02d/%04d\n", futureDate->tm_mday, futureDate->tm_mon+1, futureDate->tm_year+1900);
 
  return 0;
}
```
Output:
```
La date future est: 14/07/2020
```
Vous pouvez également utiliser cette même logique pour calculer une date dans le passé en soustrayant un certain nombre de jours, mois ou années.

## Plongée Profonde:
La capacité de calculer une date dans le futur ou le passé est possible grâce au système de dates et d'heures utilisé par les ordinateurs, appelé le "temps Unix". Ce système utilise un nombre entier représentant le nombre de secondes écoulées depuis le 1er janvier 1970 (également connu sous le nom d'epoch time). Les langages de programmation offrent des fonctions qui permettent de convertir ces timestamps en une structure de date et heure plus facilement utilisable.

Il est également possible d'utiliser d'autres librairies ou fonctions pour calculer des dates dans le futur ou le passé, comme la librairie "Date and Time" de Microsoft pour C++, ou la fonction "date_create()" de PHP.

## Voir aussi:
- [Documentation officielle sur la fonction "mktime()" en C](https://www.cplusplus.com/reference/ctime/mktime/)
- [Documentation officielle sur la librairie "Date and Time" de Microsoft pour C++](https://docs.microsoft.com/fr-fr/cpp/standard-library/date-and-time-classes?view=vs-2019)
- [Documentation officielle sur la fonction "date_create()" de PHP](https://www.php.net/manual/fr/datetime.create.php)