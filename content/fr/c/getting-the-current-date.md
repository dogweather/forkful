---
title:                "C: Obtenir la date actuelle"
simple_title:         "Obtenir la date actuelle"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Pourquoi

La récupération de la date actuelle est une tâche courante pour de nombreux programmeurs, que ce soit pour l'afficher dans une interface utilisateur ou pour suivre l'heure à laquelle une action a été effectuée. Dans cet article, nous allons explorer les différentes façons de récupérer la date actuelle en C et discuter des avantages et des inconvénients de chaque approche.

## Comment Faire

Il existe plusieurs façons de récupérer la date actuelle en C, en utilisant des fonctions prédéfinies ou en écrivant votre propre code. Jetons un coup d'œil à quelques exemples de code pour récupérer la date actuelle en utilisant différentes méthodes.

```c
// Utilisation de la fonction time()
#include <stdio.h>
#include <time.h>

int main() {
  // Déclaration d'une variable de type time_t
  time_t currentTime;
  // Utilisation de la fonction time() pour récupérer la date actuelle
  time(&currentTime);
  // Convertit la date en une chaîne de caractères
  char* dateString = ctime(&currentTime);
  // Affiche la date actuelle
  printf("%s", dateString);
  return 0;
}

/* Output:
Sun Jul 19 18:36:07 2020
*/
```

Nous pouvons également utiliser la fonction `localtime()` pour obtenir une structure de temps locale à partir de laquelle nous pouvons extraire les informations de date. Voici un exemple de code utilisant cette méthode :

```c
// Utilisation de la fonction localtime()
#include <stdio.h>
#include <time.h>

int main() {
  // Déclaration d'une variable de type time_t
  time_t currentTime;
  // Utilisation de la fonction time() pour récupérer la date actuelle
  time(&currentTime);
  // Convertit l'heure actuelle en heure locale
  struct tm *localTime = localtime(&currentTime);
  // Affiche le mois, le jour et l'année actuels
  printf("Mois : %d\nJour : %d\nAnnée : %d\n", 
    localTime->tm_mon + 1, localTime->tm_mday, localTime->tm_year + 1900);
  return 0;
}

/* Output:
Mois : 7
Jour : 19
Année : 2020
*/
```

Il est également possible de récupérer la date actuelle en utilisant la fonction `strftime()` pour formater la date selon vos préférences. Voici un exemple de code montrant comment utiliser cette fonction :

```c
// Utilisation de la fonction strftime()
#include <stdio.h>
#include <time.h>

int main() {
  // Déclaration d'une variable de type time_t
  time_t currentTime;
  // Utilisation de la fonction time() pour récupérer la date actuelle
  time(&currentTime);
  // Définit le format de la date
  char* format = "%A, %B %d, %Y";
  // Déclare une variable pour stocker la date formatée
  char dateString[50];
  // Utilise la fonction strftime() pour formater la date
  strftime(dateString, 50, format, localtime(&currentTime));
  // Affiche la date formatée
  printf("%s", dateString);
  return 0;
}

/* Output:
Sunday, July 19, 2020
*/
```

## Plongée Approfondie

Maintenant que nous avons vu quelques exemples de code pour obtenir la date actuelle, examinons de plus près certaines des fonctions que nous utilisons. La fonction `time()` renvoie le nombre de secondes écoulées depuis le 1er janvier 1970 à 00:00:00 UTC, également connu sous le nom d'époque Unix. Cette valeur est stockée dans une variable de type `time_t`.

La fonction `localtime()` convertit cette valeur de temps en une structure de type `tm` qui stocke les informations de date et d'heure en fonction du fuseau horaire local. Cette structure contient les membres suivants :

- `tm_sec` : les secondes (de 0 à 61 car il y a une seconde intercalaire tous les quelques ans).
- `tm_min` : les minutes (de 0 à 59).
- `tm_hour` : les heures (de 0 à 23).
- `tm_mday` : le jour du mois (de 1 à 31).
- `tm_mon` : le mois de l'année (de 0 à 11).