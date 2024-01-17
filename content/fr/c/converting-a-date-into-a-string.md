---
title:                "Convertir une date en une chaîne de caractères"
html_title:           "C: Convertir une date en une chaîne de caractères"
simple_title:         "Convertir une date en une chaîne de caractères"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est et pourquoi le faire?

Convertir une date en chaîne de caractères est un processus couramment utilisé en programmation qui consiste à transformer une date dans un format déterminé en une chaîne de caractères. Les programmeurs le font souvent pour des raisons d'affichage ou de manipulation de données de date.

## Comment le faire:

```C
#include <stdio.h>
#include <time.h>

int main() {
  time_t t = time(NULL);  // récupère l'heure actuelle en tant que type de données time_t
  struct tm* now = localtime(&t);  // convertit le time_t en une structure tm contenant les informations de date et heure

  char date_string[25];  // crée un tableau de caractères de taille suffisante pour contenir la date sous forme de chaîne
  strftime(date_string, sizeof(date_string), "%d/%m/%Y", now);  // utilise la fonction strftime pour formater la date selon le modèle spécifié dans le dernier argument

  printf("La date actuelle est : %s\n", date_string);  // affiche la date formatée
  
  return 0;
}
```
```
Output: La date actuelle est : 01/06/2021
```

## Plongée en profondeur:

Ce processus est souvent nécessaire lors de la manipulation de données de date dans un code. Il est également utile lorsqu'on cherche à afficher une date dans un format spécifique, selon les préférences de l'utilisateur. Avant la norme ISO C99, il n'y avait pas de fonction standard pour convertir une date en chaîne de caractères en C, mais la fonction `strftime()` a été ajoutée à cette norme et est largement utilisée pour cette tâche.

Il existe également d'autres façons d'effectuer cette conversion, telles que l'utilisation de la fonction `sprintf()` ou la manipulation manuelle de la structure tm. Cependant, la fonction `strftime()` est généralement considérée comme la meilleure option, car elle permet une plus grande flexibilité dans le formatage de la date et est plus facile à utiliser.

## Voir aussi:

- [Documentation officielle de la fonction strftime()](https://en.cppreference.com/w/c/chrono/strftime)
- [Différents formats de date en C](https://www.tutorialspoint.com/c_standard_library/time_h.htm)