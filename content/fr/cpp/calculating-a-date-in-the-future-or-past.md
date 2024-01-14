---
title:    "C++: Calculer une date dans le futur ou le passé"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/cpp/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Pourquoi

La conception d'une fonction de calcul de date dans le futur ou dans le passé peut être utile pour les programmeurs qui travaillent sur des applications ou des sites Web nécessitant des fonctionnalités de réservation de rendez-vous, de planification de tâches ou de suivi du temps. Elle peut également être utile pour tous les utilisateurs souhaitant planifier des événements à l'avance.

## Comment faire

La programmation d'une telle fonction peut sembler complexe, mais grâce à quelques connaissances de base en C++, elle peut être facilement réalisée. Voici un exemple de code pour calculer la date dans le futur avec une entrée de l'utilisateur pour le nombre de jours à ajouter :

```C++
#include <iostream>
#include <ctime>
using namespace std;

int main() {
    // Demander à l'utilisateur un nombre de jours à ajouter
    int nbJours;
    cout << "Entrez le nombre de jours à ajouter : ";
    cin >> nbJours;

    // Récupérer la date actuelle
    time_t now = time(0);

    // Convertir en structure tm pour obtenir le jour, le mois et l'année actuels
    tm *ltm = localtime(&now);

    // Ajouter le nombre de jours à la date actuelle
    ltm->tm_mday += nbJours;

    // Convertir en temps depuis l'époque
    now = mktime(ltm);

    // Convertir en structure tm pour obtenir la nouvelle date
    ltm = localtime(&now);

    // Afficher la nouvelle date
    cout << "La date dans " << nbJours << " jours sera le : ";
    cout << ltm->tm_mday << "/" << 1 + ltm->tm_mon << "/" << 1900 + ltm->tm_year << endl;

    return 0;
}
```

Voici un exemple de sortie avec une entrée de 7 jours :

> Entrez le nombre de jours à ajouter : 7
> La date dans 7 jours sera le : 6/6/2021

Pour calculer une date dans le passé, il suffit de soustraire le nombre de jours à la date actuelle.

## Plongée profonde

Derrière les lignes de code ci-dessus se cachent des concepts qui méritent une explication plus détaillée. Premièrement, la fonction `time()` renvoie le temps actuel en secondes depuis l'époque (1er janvier 1970). Ensuite, la structure `tm` est utilisée pour stocker les informations sur la date, telles que le jour, le mois et l'année. La fonction `localtime()` convertit ensuite le temps en utilisant le fuseau horaire local de l'utilisateur pour remplir cette structure. Enfin, la fonction `mktime()` convertit la structure `tm` en un nouveau temps, en utilisant à nouveau le fuseau horaire local de l'utilisateur.

De plus, ce code suppose que l'utilisateur entrera un nombre positif pour les jours, mais une validation supplémentaire peut être ajoutée pour s'assurer qu'un nombre valide est entré.

## Voir aussi

- [Calcul de durée en C++](https://www.geeksforgeeks.org/program-to-calculate-the-duration-between-two-dates-in-c-cpp/)
- [Convertir entre les différents formats de date en C++](https://www.learntodayonline.com/convert-between-different-date-formats-in-c/)
- [Gestion du temps en C++](https://www.cplusplus.com/reference/ctime/)