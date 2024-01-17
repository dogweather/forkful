---
title:                "Comparaison de deux dates"
html_title:           "Arduino: Comparaison de deux dates"
simple_title:         "Comparaison de deux dates"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/arduino/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi?
Comparer deux dates est une tâche courante pour les programmeurs qui leur permet de vérifier si une date est antérieure, égale ou postérieure à une autre date. Cela peut être utile pour des fonctions telles que la planification des tâches, la gestion des événements ou pour créer des conditions dans un programme.

## Comment Faire:
Voici un exemple de code pour comparer deux dates dans Arduino:

```
#include <Time.h>

// Définir les dates à comparer
TimeDate date1 = {2020, 8, 15, 15, 30, 0}; 
TimeDate date2 = {2020, 8, 15, 12, 0, 0};

// Comparer les dates
if(date1<date2) {
  Serial.println("Date 1 est antérieure à Date 2");
} else if(date1 == date2) {
  Serial.println("Date 1 est égale à Date 2");
} else {
  Serial.println("Date 1 est postérieure à Date 2");
}

```

La sortie du moniteur série pour cet exemple serait:

```
Date 1 est postérieure à Date 2
```

## Plongez Plus Loin:
La comparaison de dates dans Arduino est rendue possible grâce à la bibliothèque Time.h qui fournit des fonctions pour manipuler et comparer les dates et heures. Avant cette bibliothèque, les programmeurs devaient utiliser des algorithmes plus complexes pour exécuter cette tâche.

Une alternative à l'utilisation de la bibliothèque Time.h est d'utiliser la fonction `millis()` qui renvoie le nombre de millisecondes depuis le dernier redémarrage de la carte Arduino. Cela peut être utile si vous devez comparer des intervalles de temps courts plutôt que des dates précises.

En termes d'implémentation, la bibliothèque Time.h utilise la fonction `time_t` qui représente le nombre de secondes écoulées depuis le 1er janvier 1970, également connu sous le nom de "UNIX Epoch time". Cette valeur est ensuite convertie en format lisible par l'homme pour être affichée.

## Voir Aussi:
- La documentation officielle de la bibliothèque Time.h pour plus d'informations sur les fonctions et formats de date disponibles: https://www.arduino.cc/reference/en/libraries/time/
- Cette vidéo qui explique en détail comment comparer des dates dans Arduino en utilisant la bibliothèque Time.h: https://www.youtube.com/watch?v=AQBMYJMumVs