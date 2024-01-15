---
title:                "Convertir une date en chaîne de caractères"
html_title:           "C: Convertir une date en chaîne de caractères"
simple_title:         "Convertir une date en chaîne de caractères"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Pourquoi

Convertir une date en une chaîne de caractères est quelque chose de courant dans la programmation C. Cela peut être utile pour afficher une date dans un format spécifique ou pour stocker une date dans une base de données.

## Comment faire

Voici un exemple de code en utilisant la fonction `strftime()` pour convertir une date en une chaîne de caractères :

```C
#include <stdio.h>
#include <time.h>

int main()
{
  // Récupérer la date et l'heure actuelle
  time_t now;
  time(&now);

  // Convertir la date en une chaîne de caractères
  char str[100];
  strftime(str, sizeof(str), "%d/%m/%Y %H:%M:%S", localtime(&now));

  // Afficher la chaîne de caractères
  printf("La date et l'heure actuelle sont : %s\n", str);

  return 0;
}

```

Output :
```
La date et l'heure actuelle sont : 05/05/2021 14:30:00
```

## Plongée profonde

En utilisant la fonction `strftime()`, on peut spécifier le format de la chaîne de caractères de sortie en utilisant des indicateurs de formatage. Par exemple, `%d` correspond au jour du mois, `%m` au mois, `%Y` à l'année et `%H` à l'heure au format 24 heures. Il y a beaucoup d'autres indicateurs disponibles, tels que `%b` pour le mois abrégé en anglais (par exemple, May) ou `%A` pour le jour de la semaine complet en anglais (par exemple, Wednesday).

Il est important de noter que le paramètre `size` de la fonction `strftime()` doit être assez grand pour stocker la chaîne de caractères de sortie. Sinon, des erreurs peuvent survenir.

## Voir aussi

- La documentation officielle de la fonction `strftime()` : https://www.cplusplus.com/reference/ctime/strftime/
- La différence entre `time()` et `localtime()` pour récupérer la date et l'heure : https://stackoverflow.com/questions/1815035/difference-between-time-and-localtime