---
title:                "C: Obtenir la date actuelle"
programming_language: "C"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c/getting-the-current-date.md"
---

{{< edit_this_page >}}

# Pourquoi

Obtenir la date courante est une tâche courante dans de nombreux programmes C. Cela peut être utile pour les applications telles que les systèmes de gestion de fichiers, les applications de calendrier et les sauvegardes de données régulières. En utilisant la bibliothèque standard de C, il est facile d'obtenir la date actuelle dans votre code.

# Comment Faire

Pour obtenir la date et l'heure actuelles dans un programme C, nous utiliserons les fonctions fournies par la bibliothèque "time.h". Tout d'abord, nous devons inclure cette bibliothèque en utilisant la directive "#include" :

```C
#include <time.h>
```

Ensuite, nous pouvons utiliser la fonction "time" pour obtenir le nombre de secondes écoulées depuis le 1er janvier 1970 :

```C
time_t currentTime = time(NULL);
```

Nous pouvons ensuite utiliser cette valeur pour obtenir une structure "tm" qui contient l'heure et la date actuelles en utilisant la fonction "localtime" :

```C
struct tm *localTime = localtime(&currentTime);
```

Enfin, nous pouvons accéder aux différents éléments de la date et de l'heure en utilisant les champs de la structure "tm". Par exemple, pour afficher l'heure actuelle au format 24 heures, nous pouvons utiliser la fonction "strftime" :

```C
char timeString[10];
strftime(timeString, sizeof(timeString), "%H:%M:%S", localTime);
printf("Il est %s", timeString);
```

Ce code produira une sortie similaire à ceci : "Il est 15:23:46".

# Plongée Profonde

La fonction "time" renvoie le nombre de secondes écoulées depuis le 1er janvier 1970, également appelé "epoch". Cette méthode de calcul du temps est appelée "epoch time" et est largement utilisée dans les systèmes Unix et Windows.

La fonction "localtime" convertit le nombre de secondes en une structure "tm" qui contient des informations sur la date et l'heure locales telles que l'année, le mois, le jour, l'heure, le minute, etc.

La fonction "strftime" convertit ensuite la structure "tm" en une chaîne de caractères au format spécifié. Ici, nous avons utilisé "%H:%M:%S" pour afficher l'heure au format 24 heures.

Il est important de noter que ces fonctions utilisent l'heure locale du système, donc si vous souhaitez obtenir l'heure dans un fuseau horaire différent, vous devrez utiliser une fonction différente telle que "gmtime".

# Voir Aussi

- [Documentation de la bibliothèque de temps en C](https://en.wikipedia.org/wiki/C_date_and_time_functions)
- [Guide complet de la bibliothèque de temps en C](https://www.tutorialspoint.com/c_standard_library/c_function_localtime.htm)
- [Fonction "time" sur cppreference.com](https://en.cppreference.com/w/c/chrono/time)