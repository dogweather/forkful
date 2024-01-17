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

## Qu'est-ce que c'est et pourquoi le faire?

Obtenir la date et l'heure actuelles est une opération courante dans le développement de logiciels. Cela permet aux programmeurs de savoir quand un certain événement a eu lieu ou de mesurer le temps écoulé depuis un moment précis. C'est également utile pour afficher la date et l'heure actuelles dans une application.

## Comment faire:

Les langages de programmation comme C offrent des fonctionnalités intégrées pour obtenir la date et l'heure actuelles. Voici un exemple de code en C pour obtenir la date actuelle:

```C
#include <stdio.h>
#include <time.h>

int main() {
    time_t temps;
    struct tm *actuel;
    char date[100];

    temps = time(NULL);
    actuel = localtime(&temps);

    strftime(date, sizeof(date), "%A %d %B, %Y", actuel);

    printf("La date actuelle est: %s\n", date);

    return 0;
}
```

Voici un exemple de sortie pour le code ci-dessus:

```
La date actuelle est: Mardi 26 Janvier, 2021
```

## Plongée en profondeur:

La fonction `time()` en C renvoie le nombre de secondes écoulées depuis le 1er janvier 1970. Ce concept est connu sous le nom de temps Unix ou Epoch time. Le moment zéro de la mesure du temps Unix est le 1er janvier 1970 à 00:00:00 UTC.

Il existe plusieurs alternatives pour obtenir la date actuelle en C, telles que `localtime()` qui retourne une structure avec des informations détaillées sur la date et l'heure actuelles, ou `gmtime()` pour obtenir l'heure universelle coordonnée (UTC).

Dans certaines situations, il peut être nécessaire de modifier la valeur de la variable d'environnement `TZ` pour obtenir la date et l'heure dans un fuseau horaire spécifique.

## Voir aussi:

- [La documentation officielle de la fonction time() en C](https://en.cppreference.com/w/c/chrono/time)
- [Un tutoriel sur l'utilisation de time() en C](https://www.tutorialspoint.com/c_standard_library/c_function_time.htm)
- [Informations sur Epoch time et la mesure du temps Unix](https://www.epochconverter.com/)