---
title:                "C: Calcul d'une date dans le futur ou le passé"
programming_language: "C"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Pourquoi

Dans le développement de logiciels, il est parfois nécessaire de calculer des dates dans le futur ou le passé. Cela peut être utile pour planifier des événements, gérer des tâches à exécuter ou même pour des raisons de débogage.

## Comment faire

Pour calculer une date dans le futur, vous aurez besoin des informations suivantes:
- La date actuelle
- Le nombre de jours à ajouter à la date actuelle

Utilisant ces informations, vous pouvez utiliser les fonctions de la bibliothèque *time.h* pour ajouter les jours à la date actuelle et obtenir la date souhaitée. Voici un exemple de code en utilisant ces fonctions:

```C
#include <stdio.h>
#include <time.h>

int main() {
  // Obtenez la date actuelle
  time_t t = time(NULL);
  struct tm *today = localtime(&t);
  
  // Ajouter 5 jours à la date actuelle
  today->tm_mday += 5;
  
  // Convertir en temps UNIX
  time_t future_date = mktime(today);

  // Imprimer la date dans un format lisible
  printf("La date dans 5 jours est: %s", ctime(&future_date));

  return 0;
}
```

La sortie de ce programme sera:

```
La date dans 5 jours est: Sat Aug 22 14:26:05 2020
```

Pour calculer une date dans le passé, le principe est le même, sauf que vous devez soustraire les jours au lieu de les ajouter. Voici un exemple de code:

```C
#include <stdio.h>
#include <time.h>

int main() {
  // Obtenez la date actuelle
  time_t t = time(NULL);
  struct tm *today = localtime(&t);

  // Soustraire 10 jours à la date actuelle
  today->tm_mday -= 10;

  // Convertir en temps UNIX
  time_t past_date = mktime(today);

  // Imprimer la date dans un format lisible
  printf("La date il y a 10 jours était: %s", ctime(&past_date));

  return 0;
}
```

La sortie de ce programme sera:

```
La date il y a 10 jours était: Thu Aug 07 14:28:21 2020
```

## Plongée en profondeur

Calculer une date dans le futur ou le passé peut sembler simple, mais il y a quelques points à prendre en compte pour obtenir des résultats précis.

Tout d'abord, il est important de comprendre que les dates sont stockées en temps UNIX, qui est le nombre de secondes écoulées depuis le 1 janvier 1970. Cela signifie que si vous ajoutez ou soustrayez des jours, vous modifiez également le temps en secondes et donc le fuseau horaire. Par conséquent, il est important de convertir la date en temps UNIX avant de l'ajuster et de la reconvertir en une date lisible. Vous pouvez également utiliser les fonctions de la bibliothèque *localtime* ou *gmtime* pour ajuster le fuseau horaire en conséquence.

De plus, faites attention aux dates spéciales telles que les années bissextiles. Vous devrez peut-être prendre en compte ces détails lors du calcul de la date.

## Voir aussi

- [Tutoriel C: Gestion de dates et d'horloges](https://www.tutorialspoint.com/c_standard_library/c_function_strftime.htm)
- [Documentation officielle de la bibliothèque time.h](https://pubs.opengroup.org/onlinepubs/9699919799/basedefs/time.h.html)