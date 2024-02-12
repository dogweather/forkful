---
title:                "Calculer une date dans le futur ou le passé"
aliases:
- /fr/c/calculating-a-date-in-the-future-or-past/
date:                  2024-02-03T17:53:06.434955-07:00
model:                 gpt-4-0125-preview
simple_title:         "Calculer une date dans le futur ou le passé"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c/calculating-a-date-in-the-future-or-past.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?
Calculer une date dans le futur ou le passé consiste à déterminer une date spécifique en ajoutant ou en soustrayant un certain nombre de jours, de mois ou d'années à une date donnée. Les programmeurs font cela pour des tâches telles que la planification d'événements, la génération de rappels ou la gestion des dates d'expiration, ce qui en fait une fonctionnalité essentielle dans diverses applications, allant des systèmes de calendrier au logiciel financier.

## Comment faire :
Bien que la bibliothèque standard du C ne fournisse pas de fonctions directes pour l'arithmétique des dates, vous pouvez manipuler les dates en utilisant la bibliothèque `time.h`, en travaillant spécifiquement avec le type de données `time_t` et `struct tm`. Voici un exemple simplifié de comment ajouter des jours à la date actuelle :

```c
#include <stdio.h>
#include <time.h>

void addDays(struct tm* date, int daysToAdd) {
    const time_t ONE_DAY = 24 * 60 * 60; // secondes dans un jour
    // Convertir la structure tm en time_t, ajouter les jours, et convertir en retour
    time_t date_seconds = mktime(date) + (daysToAdd * ONE_DAY);
    *date = *localtime(&date_seconds);
}

int main() {
    time_t now;
    time(&now);
    struct tm futureDate = *localtime(&now);

    int daysToAdd = 10; // Ajustez cela pour les jours souhaités à ajouter
    addDays(&futureDate, daysToAdd);

    printf("Date future : %d-%d-%d\n", futureDate.tm_year + 1900, futureDate.tm_mon + 1, futureDate.tm_mday);

    return 0;
}
```

Ce code ajoute un nombre spécifié de jours à la date actuelle et imprime la date future. Notez que cette approche prend en compte les secondes intercalaires et les ajustements de l'heure d'été, comme gérés par `mktime` et `localtime`.

Exemple de sortie :

```
Date future : 2023-04-23
```

Gardez à l'esprit que cet exemple ajoute des jours, mais avec des calculs plus complexes (comme des mois ou des années, en tenant compte des années bissextiles), vous auriez besoin d'une logique plus sophistiquée ou de bibliothèques comme `date.h` en C++ ou de bibliothèques tierces en C.

## Exploration approfondie
Manipuler les dates en C en utilisant la bibliothèque time.h implique la manipulation directe du temps en secondes depuis l'époque Unix (00:00, le 1er janvier 1970, UTC), suivie de la conversion de ces secondes en un format de date plus lisible par l'homme (`struct tm`). Cette approche est simple mais efficace pour les opérations de base et bénéficie d'être multiplateforme et de faire partie de la bibliothèque standard du C.

Cependant, la simplicité de cette méthode est aussi une limitation. Traiter des calculs de date plus complexes (tels que tenir compte des longueurs de mois variables, des années bissextiles et des fuseaux horaires) devient rapidement non trivial. Des langues comme Python avec `datetime` ou Java avec `java.time` fournissent des API plus intuitives pour l'arithmétique des dates, embrassant les principes orientés objet pour la clarté et la facilité d'utilisation.

En pratique, lorsqu'on travaille sur des projets nécessitant une manipulation extensive des dates en C, les développeurs se tournent souvent vers des bibliothèques tierces pour des solutions plus robustes. Ces bibliothèques peuvent offrir des fonctionnalités de date et d'heure complètes, y compris la gestion des fuseaux horaires, des options de formatage et des capacités d'arithmétique de date plus nuancées, simplifiant considérablement la tâche du développeur.

Malgré la disponibilité d'alternatives plus modernes, comprendre comment manipuler les dates en utilisant la bibliothèque standard du C reste une compétence précieuse. Elle offre des perspectives approfondies sur la manière dont les ordinateurs représentent et travaillent avec le temps, un concept fondamental qui transcende les langues de programmation spécifiques.
