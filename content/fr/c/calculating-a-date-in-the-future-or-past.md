---
title:                "Calculer une date dans le futur ou le passé"
date:                  2024-01-20T17:28:33.425418-07:00
model:                 gpt-4-1106-preview
html_title:           "C: Calculer une date dans le futur ou le passé"
simple_title:         "Calculer une date dans le futur ou le passé"

category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?
Calculer une date dans le futur ou le passé, c'est juste déterminer une date en ajoutant ou en soustrayant des jours à une date existante. En programmation, cela sert à évaluer des délais, programmer des événements ou des tâches récurrentes.

## Comment faire :
```C
#include <stdio.h>
#include <time.h>

int main() {
    time_t la_date = time(NULL);           // Date actuelle
    struct tm new_date = *localtime(&la_date);

    // Calculer une date 10 jours dans le futur
    new_date.tm_mday += 10;                // Ajouter 10 jours
    mktime(&new_date);                     // Normaliser la date

    // Afficher la nouvelle date
    printf("La date dans 10 jours sera : %d-%02d-%02d\n",
        new_date.tm_year + 1900,           // L'année (depuis 1900)
        new_date.tm_mon + 1,               // Le mois (0-11)
        new_date.tm_mday);                 // Le jour

    return 0;
}
```
Sample output:
```
La date dans 10 jours sera : 2023-04-14
```

## Exploration approfondie :
Initialement, les dates étaient calculées manuellement, mais avec l'avènement des ordinateurs, ce processus a été grandement simplifié et automatisé. En C, on utilise souvent la librairie `<time.h>` pour gérer les dates et les heures. Cette librairie fournit des structures et des fonctions pour manipuler le temps de manière standardisée.

Il existe aussi d'autres approches comme les bibliothèques de date de haute précision ou celles gérant spécifiquement les calendriers grégorien ou julien, mais `<time.h>` reste une solution robuste et suffisante pour la plupart des besoins de base.

Un détail important lors de l'implémentation est de toujours normaliser la date après modification avec `mktime()`, cela ajuste tous les champs de la structure `tm` au cas où les nouveaux valeurs seraient en dehors des limites usuels, comme le 32ème jour d'un mois.

## Voir également :
- [Open Group - Specification of `time.h`](https://pubs.opengroup.org/onlinepubs/009695399/basedefs/time.h.html)
- [Tutorialspoint - C Standard Library `<time.h>`](https://www.tutorialspoint.com/c_standard_library/time_h.htm)
