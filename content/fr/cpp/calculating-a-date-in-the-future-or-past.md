---
title:                "C++: Calculer une date dans le futur ou le passé."
programming_language: "C++"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/cpp/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Pourquoi

La programmation est un outil incroyablement utile dans de nombreux domaines, y compris le suivi des dates et le calcul de dates futures ou antérieures. Cela peut être particulièrement utile lors de la planification d'événements ou de tâches à long terme.

## Comment Faire

Pour calculer une date dans le futur ou le passé en utilisant C++, nous pouvons utiliser la bibliothèque standard <code>ctime</code> et la fonction <code>mktime()</code>. Jetons un coup d'oeil à un exemple de code pour calculer une date dans le futur:

```C++
#include <iostream>
#include <ctime>

int main() {
    // Obtenez la date actuelle
    time_t now;
    time(&now);
    
    // Structure tm pour stocker la date et le temps
    tm *date = localtime(&now);
    
    // Ajouter 3 mois à la date actuelle
    date->tm_mon += 3;
    
    // Convertir en nouveau timestamp
    time_t future = mktime(date);
    
    // Afficher la date dans le futur
    std::cout << "La date dans 3 mois sera le " <<
    asctime(localtime(&future));
    
    return 0;
}
```

La sortie de ce code sera:

```
La date dans 3 mois sera le Thu Jul 01 00:00:00 2021
```

Pour calculer une date dans le passé, il suffit de soustraire un certain nombre de jours, mois ou années à la date actuelle.

## Plongée Profonde

La fonction <code>mktime()</code> utilise le type <code>time_t</code> qui représente le temps écoulé depuis minuit du 1er janvier 1970. Cela signifie que pour calculer une date dans le futur ou le passé, nous devons d'abord obtenir la date actuelle en tant que <code>time_t</code> et l'ajuster en conséquence.

De plus, la fonction <code>localtime()</code> convertit le <code>time_t</code> en une structure <code>tm</code>, qui stocke des informations telles que le jour, le mois, l'année, etc. Nous pouvons ainsi facilement modifier ces valeurs pour calculer une date future ou passée.

## Voir Aussi

- Documentation sur la bibliothèque <code>ctime</code>
- Tutoriel sur la manipulation des dates en C++
- Cours sur la programmation en C++: Les Dates