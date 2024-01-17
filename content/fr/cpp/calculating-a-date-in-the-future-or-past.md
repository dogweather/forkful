---
title:                "Calculer une date dans le futur ou le passé"
html_title:           "C++: Calculer une date dans le futur ou le passé"
simple_title:         "Calculer une date dans le futur ou le passé"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/cpp/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# Qu'est-ce et pourquoi?

Calculer une date dans le passé ou le futur est une tâche courante dans la programmation. Cela consiste à trouver une nouvelle date en ajoutant ou en soustrayant un certain nombre de jours, de semaines, de mois ou d'années à une date existante. Les programmeurs le font souvent pour planifier des événements, pour suivre des délais ou pour effectuer des tâches récurrentes à une date spécifique.

# Comment faire:

Voici un exemple de code simple en ```C++``` pour calculer une date dans le futur en ajoutant 10 jours à la date actuelle:

```
#include <iostream>
#include <chrono>

using namespace std;

int main()
{
    // Date actuelle
    chrono::system_clock::time_point now = chrono::system_clock::now();

    // Ajout de 10 jours à la date actuelle
    chrono::hours hrs(24 * 10);
    chrono::system_clock::time_point future_date = now + hrs;

    // Conversion en forme lisible
    time_t tt = chrono::system_clock::to_time_t(future_date);
    tm *ptm = localtime(&tt);
    cout << "La date dans 10 jours sera : " << ptm->tm_mday << "/" << (ptm->tm_mon+1) << "/" << (ptm->tm_year+1900) << endl;

    return 0;
}
```
**Sortie:**
```
La date dans 10 jours sera : 2/9/2020
```

# Plongeons plus profondément:

- Contexte historique: Calculer une date dans le futur ou le passé a été une tâche difficile avant l'invention des ordinateurs. Les gens utilisaient des calendriers et des formules mathématiques pour le faire.
- Alternatives: En plus des opérations arithmétiques, les programmeurs peuvent également utiliser des bibliothèques spéciales pour la manipulation de dates, telles que ```boost::date_time```.
- Détails d'implémentation: Le code ci-dessus utilise la bibliothèque standard ```chrono``` pour effectuer des opérations sur les dates. Il utilise également la fonction ```localtime``` pour convertir la date en une forme lisible.

# Voir également:

- [Tutorial sur la manipulation de dates en C++](https://www.learncpp.com/cpp-tutorial/51-working-with-dates-and-times/)
- [Documentation sur la bibliothèque standard «chrono»](https://en.cppreference.com/w/cpp/chrono)