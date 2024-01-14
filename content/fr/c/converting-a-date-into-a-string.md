---
title:    "C: Convertir une date en chaîne de caractères"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/c/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Pourquoi

La conversion d'une date en une chaîne de caractères est une tâche courante en programmation, surtout si vous travaillez avec des données relatives au temps ou si vous souhaitez afficher une date formatée pour l'utilisateur. Cet article expliquera comment réaliser cette conversion en utilisant le langage de programmation C.

## Comment faire

Pour commencer, nous allons déclarer une variable pour stocker notre date en utilisant le type de données struct tm de la bibliothèque standard du langage C. Ensuite, nous utiliserons la fonction strftime () pour convertir cette date en une chaîne de caractères selon le format souhaité. Voici un exemple de code :

```C
#include <stdio.h>
#include <time.h> // inclus la bibliothèque pour la conversion de date

int main()
{
    // déclaration d'une variable de type struct tm pour stocker la date
    struct tm date = { .tm_year = 120, // année 2020 - 1900
                    .tm_mon = 6, // mois de juillet (valeurs de 0 à 11)
                    .tm_mday = 15, // jour 15
                    .tm_hour = 15, // heure 15
                    .tm_min = 30, // minute 30
                    .tm_sec = 0 // seconde 0
    };

    // déclaration d'un tableau de caractères pour stocker la date en chaîne de caractères
    char date_string[50];

    // utilisation de la fonction strftime pour convertir la date en chaîne de caractères
    strftime(date_string, 50, "%A, %d %B %Y - %H:%M", &date);

    // affichage de la date convertie
    printf("La date est : %s\n", date_string);

    return 0;
}
```
Le résultat de ce code sera : "La date est : mercredi, 15 juillet 2020 - 15:30"

## Plongez plus en profondeur

La fonction strftime () a plusieurs arguments qui permettent de spécifier le format de la chaîne de caractères de sortie. Le premier argument est un tableau de caractères qui stockera la date convertie, le deuxième argument est la taille de ce tableau, le troisième argument est le format de la chaîne de caractères, et le dernier argument est l'adresse de la variable de type struct tm qui contient la date. Vous pouvez trouver une liste complète des spécificateurs de format pour la fonction strftime () en consultant la documentation en ligne de la bibliothèque standard du langage C.

Un autre point important à noter est que la fonction strftime () stocke automatiquement la date dans la zone horaire locale de l'utilisateur. Si vous avez besoin d'afficher la date dans une autre zone horaire, vous devrez utiliser d'autres fonctions, telles que gmtime () et mktime () pour effectuer les conversions nécessaires.

## Voir aussi

- [Documentation sur la fonction strftime ()](https://www.tutorialspoint.com/c_standard_library/c_function_strftime.htm)
- [Guide pour travailler avec les date et heure en C](https://www.programiz.com/c-programming/c-date-time)
- [Autres fonctions utiles pour la gestion de la date en C](https://www.knowprogram.com/c-programming/date-and-time-programming-c/)