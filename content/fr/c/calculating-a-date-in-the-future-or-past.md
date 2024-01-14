---
title:                "C: Calculer une date dans le futur ou le passé"
simple_title:         "Calculer une date dans le futur ou le passé"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Pourquoi

Calculer une date dans le passé ou dans le futur peut être extrêmement utile en programmation. Cela peut être utilisé pour automatiser des tâches telles que la planification des rappels ou la création de calendriers.

## Comment faire

La première étape pour calculer une date dans le futur ou dans le passé est de déterminer quelle date de référence vous souhaitez utiliser. Cela peut être la date en cours ou une date spécifique définie par l'utilisateur.

Ensuite, vous devez déterminer le nombre de jours à ajouter ou soustraire à cette date de référence. Vous pouvez utiliser la fonction `time()` pour obtenir le nombre de secondes écoulées depuis le 1er janvier 1970 et le diviser par le nombre de secondes dans une journée (86400) pour obtenir le nombre de jours.

Enfin, vous pouvez utiliser les fonctions `gmtime()` et `asctime()` pour convertir le nombre de jours en une structure de date et l'afficher dans un format lisible pour l'utilisateur.

Voici un exemple de code en C pour calculer une date dans le futur et l'afficher au format DD/MM/AAAA :

```C
#include <stdio.h>
#include <time.h>

int main() {
  // Date actuelle
  struct tm *date_actuelle;
  time_t temps_actuel = time(NULL);
  date_actuelle = localtime(&temps_actuel);
  
  // Nombre de jours à ajouter
  int jours = 5;
  
  // Calcul de la date dans le futur
  date_actuelle->tm_mday += jours;
  time_t temps_futur = mktime(date_actuelle);
  date_actuelle = localtime(&temps_futur);
  
  // Affichage de la date au format DD/MM/AAAA
  printf("Date dans %d jours : %02d/%02d/%d\n", jours, date_actuelle->tm_mday, date_actuelle->tm_mon + 1, date_actuelle->tm_year + 1900);
  
  return 0;
}
```

Voici la sortie de ce programme :

```
Date dans 5 jours : 03/01/2022
```

## Plongée en profondeur

Le calcul d'une date dans le futur ou dans le passé peut sembler simple, mais il y a plusieurs facteurs à prendre en compte. Par exemple, il peut être nécessaire de vérifier si l'année est bissextile pour ajuster le nombre de jours dans le mois de février.

De plus, il est important de noter que ce calcul peut varier selon les langages de programmation et les bibliothèques utilisées. Par exemple, en utilisant la bibliothèque `<chrono>` en C++, le calcul de la date dans le futur peut être effectué en utilisant des objets de type `duration` et `time_point`.

Il est également important de se rappeler que le temps et les dates sont souvent sujets à des différences et des complications en raison de facteurs tels que les fuseaux horaires et les changements d'heure.

## Voir aussi

- [Documentation officielle sur la bibliothèque <time.h> en C](https://www.tutorialspoint.com/c_standard_library/time_h.htm)
- [Calcul d'une date dans le futur à l'aide de la bibliothèque <chrono> en C++](https://www.geeksforgeeks.org/get-date-time-in-seconds-using-c-chrono-library/)
- [Différences entre les fuseaux horaires en programmation](https://www.howtogeek.com/323390/how-to-handle-time-zones-in-computer-programs/)