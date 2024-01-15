---
title:                "Obtenir la date actuelle"
html_title:           "C: Obtenir la date actuelle"
simple_title:         "Obtenir la date actuelle"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Pourquoi

Bonjour les programmeurs ! Vous êtes peut-être curieux de savoir comment obtenir la date actuelle dans vos programmes en C. Eh bien, cela peut sembler simple, mais cela peut avoir de nombreuses utilisations pratiques, comme enregistrement de fichiers, création de journaux ou tout simplement afficher la date à des fins de débogage. Alors, continuons et découvrons comment le faire !

## Comment faire

Pour obtenir la date actuelle en C, nous avons besoin d'utiliser la fonction `time()` de la bibliothèque `time.h`. Cette fonction renvoie le temps écoulé depuis le 1er janvier 1970 à 00:00:00 UTC en secondes. Ensuite, nous pouvons utiliser la fonction `localtime()` pour convertir le temps en une structure `tm` qui contient les informations sur la date et l'heure locale.

Voici un exemple de code pour obtenir la date actuelle en utilisant les fonctions mentionnées ci-dessus :

```C
#include <stdio.h>
#include <time.h>

int main()
{
    // Obtention du temps actuel
    time_t temps_actuel = time(0);
    
    // Conversion en une structure tm
    struct tm *temps_local = localtime(&temps_actuel);
    
    // Affichage de la date actuelle
    printf("La date actuelle est : %d/%d/%d\n", temps_local->tm_mday, temps_local->tm_mon + 1, temps_local->tm_year + 1900);
    
    return 0;
}
```

La sortie de ce code serait :

```
La date actuelle est : 5/1/2021
```

Remarquez que les mois sont numérotés à partir de 0, d'où l'incrémentation de 1 dans l'affichage. De plus, l'année est représentée en nombre d'années depuis 1900.

## Plongée en profondeur

Si vous êtes curieux de savoir comment la fonction `time()` parvient à donner le temps en secondes depuis 1970, voici quelques informations intéressantes. La fonction utilise l'horloge interne de l'ordinateur, qui est surtout basée sur le nombre de cycles du processeur. Cela signifie que si votre processeur est plus rapide, la valeur de retour de `time()` augmentera plus rapidement.

Ensuite, la fonction `localtime()` utilise les informations du fuseau horaire de votre ordinateur pour convertir le temps en une date et une heure locales. Cette information peut même être modifiée si vous changez manuellement le fuseau horaire de votre système.

## Voir aussi

Pour en savoir plus sur la fonction `time()` et ses utilisations, vous pouvez consulter la documentation officielle de la bibliothèque `time.h` [ici](https://www.tutorialspoint.com/c_standard_library/time_h.htm). Vous pouvez également jeter un coup d'œil à la liste complète des fonctions disponibles pour travailler avec le temps en C [ici](https://www.gnu.org/software/libc/manual/html_node/Date-and-Time.html). Bon codage !