---
title:    "C: Obtenir la date actuelle"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/c/getting-the-current-date.md"
---

{{< edit_this_page >}}

# Pourquoi

Lors de la création d'un programme en C, il est souvent nécessaire de connaître la date actuelle. Cela peut être utile pour diverses tâches, telles que la gestion de fichiers, la journalisation ou encore la création de rappels automatiques. Dans cet article, nous allons voir comment obtenir la date actuelle en langage C.

# Comment faire

Pour obtenir la date actuelle en langage C, nous allons utiliser la fonction `time` de la bibliothèque standard `time.h`. Cette fonction renvoie le temps écoulé en secondes depuis le 1er janvier 1970 à minuit (également appelé timestamp). Voici un exemple de code pour obtenir le timestamp actuel :

```C
#include <stdio.h>
#include <time.h>

int main(void) {
    time_t now = time(NULL);
    printf("Timestamp actuel : %ld\n", now);
    return 0;
}
```

En utilisant la fonction `localtime`, nous pouvons convertir le timestamp en une structure `tm` contenant des informations sur la date actuelle. Voici un exemple de code pour obtenir la date actuelle au format jour/mois/année :

```C
#include <stdio.h>
#include <time.h>

int main(void) {
    time_t now = time(NULL);
    struct tm *date = localtime(&now);
    printf("Date actuelle : %02d/%02d/%04d\n", date->tm_mday, date->tm_mon + 1, date->tm_year + 1900);
    return 0;
}
```

Le code ci-dessus utilise les valeurs des membres de la structure `tm` pour afficher la date au format souhaité. La fonction `localtime` prend en paramètre l'adresse du timestamp, ce qui explique l'utilisation de `&now`.

# Plongée en profondeur

Si vous souhaitez aller plus loin dans la gestion de la date en langage C, il existe d'autres fonctions et structures disponibles dans la bibliothèque `time.h`. Par exemple, la fonction `strftime` permet de formater la date et l'heure selon un modèle spécifique. La structure `timeval` peut être utilisée pour stocker des valeurs de temps plus précises, en incluant les microsecondes.

Il est également important de noter que le temps écoulé depuis le 1er janvier 1970 à minuit peut être différent selon les systèmes d'exploitation. Cela peut entraîner des différences de quelques secondes en fonction du système utilisé.

# Voir aussi

- [Documentation de la bibliothèque time.h en français](https://www.cprogramming.com/tutorial/time.h.html)
- [Fonctions de gestion de date en langage C](https://www.tutorialspoint.com/c_standard_library/c_function_time.htm)
- [Article sur les timestamps et la conversion en date en langage C](https://www.guru99.com/c-date-time.html)