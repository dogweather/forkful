---
title:    "C: Obtenir la date actuelle"
keywords: ["C"]
---

{{< edit_this_page >}}

## Pourquoi

La date est un élément essentiel dans la vie quotidienne et dans le développement de logiciels. Il est donc important pour les programmeurs de savoir comment obtenir la date actuelle dans leurs programmes en C. Cet article expliquera comment obtenir la date actuelle en utilisant des fonctions disponibles dans la bibliothèque de temps en langage C.

## Comment faire

Pour obtenir la date actuelle en langage C, nous utiliserons la fonction `time.h`. Cette fonction renvoie le nombre de secondes écoulées depuis le 1er janvier 1970 (également connue sous le nom de "epoch time"). Nous pouvons ensuite utiliser la fonction `localtime()` pour convertir ce nombre de secondes en une structure de date et heure locale.

Voici un exemple de code pour obtenir la date actuelle et l'afficher sous un format lisible par l'utilisateur :

```
#include <stdio.h>
#include <time.h>

int main() {
    // Obtenir le temps actuel
    time_t timestamp = time(NULL);

    // Convertir en structure de date et heure locale
    struct tm *date_time = localtime(&timestamp);

    // Afficher la date et l'heure actuelles
    printf("La date actuelle est : %02d/%02d/%d\n", date_time->tm_mday, date_time->tm_mon + 1, date_time->tm_year + 1900);
    printf("L'heure actuelle est : %02d:%02d:%02d\n", date_time->tm_hour, date_time->tm_min, date_time->tm_sec);

    return 0;
}
```

La sortie de ce code sera similaire à ceci :

```
La date actuelle est : 20/11/2021
L'heure actuelle est : 14:45:00
```

## Profonde plongée

En utilisant la fonction `localtime()`, nous pouvons également accéder aux différentes parties de la structure de date et heure locale. Par exemple, `tm_mday` représente le jour du mois, `tm_mon` représente le mois (en commençant par 0 pour janvier) et `tm_year` représente l'année depuis 1900. Vous pouvez trouver la liste complète des membres de la structure `tm` en consultant la documentation de la bibliothèque de temps en langage C.

Il est également possible de modifier la structure de date et heure locale pour afficher la date et l'heure dans un format différent. Par exemple, vous pouvez utiliser la fonction `strftime()` pour formater la date et l'heure selon vos préférences.

## Voir aussi

- [Documentation de la bibliothèque de temps en langage C](https://www.tutorialspoint.com/c_standard_library/time_h.htm)
- [Guide de référence pour la fonction `localtime()` en langage C](https://www.cplusplus.com/reference/ctime/localtime/)
- [Exemple de code pour formater la date en langage C](https://www.tutorialspoint.com/c_standard_library/c_function_strftime.htm)