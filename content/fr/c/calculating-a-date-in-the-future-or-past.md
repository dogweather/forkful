---
title:                "Calculer une date dans le futur ou le passé"
html_title:           "C: Calculer une date dans le futur ou le passé"
simple_title:         "Calculer une date dans le futur ou le passé"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi?

Calculer une date future ou passée c'est trouver une date qui est à une certaine durée d'une date donnée. Les programmeurs l'utilisent pour programmer des événements, des rappels et pour des calculs liés au temps.

## Comment faire:

Nous allons utiliser la bibliothèque time.h en C pour accomplir cette tâche. Prenons un exemple:

```C
#include <stdio.h>
#include <time.h>

int main(){
    time_t now;
    struct tm newdate;
    time(&now);
    newdate = *localtime(&now);

    newdate.tm_mday += 5; // ajoute 5 jours à la date actuelle
    mktime(&newdate);

    printf("La nouvelle date est : %s", asctime(&newdate));
    return 0; 
}
```
Lors de l'exécution, cela pourrait donner une sortie similaire à:
```C
La nouvelle date est : Mon Dec 20 12:45:50 2021
```

## Deep Dive

Historiquement, manipuler des dates et du temps a toujours été un défi dans la programmation car il faut gérer des années bissextiles, des fuseaux horaires, etc. La struct tm de la bibliothèque time.h a rendu cela beaucoup plus facile en C en définissant une structure qui facilite la manipulation du temps.

Il existe d'autres alternatives pour calculer une date future ou passée, comme la fonction localtime de POSIX qui renvoie une structure struct tm. On pourrait aussi créer une fonction personnalisée pour plus de contrôle.

N'oubliez pas que la structure tm et les fonctions associées établissent le mois à partir de zéro pour janvier, alors pensez à ajouter ou soustraire 1 lors de la manipulation des mois.

## Voir Aussi:

Pour de l'information supplémentaire, vous pouvez consulter: