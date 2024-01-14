---
title:                "C: Transformer une date en chaîne de caractères."
simple_title:         "Transformer une date en chaîne de caractères."
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Pourquoi

Il est courant dans les programmes informatiques de devoir gérer des dates et d'afficher ces dates sous forme de chaînes de caractères. La conversion d'une date en une chaîne de caractères peut sembler banale, mais c'est en fait une étape cruciale pour présenter les informations de manière lisible pour les utilisateurs.

## Comment faire

Pour convertir une date en une chaîne de caractères en C, il existe plusieurs options. Voici quelques exemples de code qui peuvent être utilisés:

```C
#include <stdio.h>
#include <time.h>

int main() {
    // Obtenir la date et l'heure actuelles 
    time_t now = time(NULL);

    // Convertir la date en chaîne de caractères 
    char* date = ctime(&now);

    // Afficher la date 
    printf("La date et l'heure actuelles sont: %s", date);

    return 0;
}
```

En utilisant la fonction ctime de la bibliothèque time, nous pouvons facilement convertir la date en une chaîne de caractères lisible.

```C
#include <stdio.h>
#include <time.h>

int main() {
    // Déclarer une structure tm pour stocker la date et l'heure 
    struct tm date;

    // Remplir la structure avec la date et l'heure souhaitées 
    date.tm_mday = 1; // jour 
    date.tm_mon = 0; // mois 
    date.tm_year = 2020; // année 
    date.tm_hour = 11; // heure 
    date.tm_min = 30; // minutes 
    date.tm_sec = 0; // secondes 

    // Convertir la date en chaîne de caractères 
    char date_str[30];
    strftime(date_str, sizeof(date_str), "%A, %d %B %Y, %H:%M:%S", &date);

    // Afficher la date 
    printf("La date et l'heure choisies sont: %s", date_str);

    return 0;
}
```

En utilisant la fonction strftime, nous pouvons formater la date selon notre préférence avant de la convertir en chaîne de caractères.

## Plongée en profondeur

La conversion d'une date en une chaîne de caractères peut paraître simple, mais il y a en réalité beaucoup de choses à prendre en compte. Par exemple, le format de la date peut varier en fonction des préférences régionales ou de la langue de l'utilisateur. Il est donc important de comprendre les différentes fonctions et options disponibles pour la conversion de dates en chaînes de caractères en C.

## Voir aussi

- [Documentation officielle de la bibliothèque time en C](https://www.gnu.org/software/libc/manual/html_node/Date-and-Time-Functions.html)
- [Documentation officielle de la fonction strftime en C](https://www.tutorialspoint.com/c_standard_library/c_function_strftime.htm)
- [Différents formats de date en C](https://www.tutorialspoint.com/c_standard_library/c_function_strftime.html)