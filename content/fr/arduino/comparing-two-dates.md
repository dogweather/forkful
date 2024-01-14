---
title:                "Arduino: Comparaison de deux dates"
simple_title:         "Comparaison de deux dates"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/arduino/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Pourquoi

L'utilisation de dates est une composante courante de nombreux projets Arduino. Il est important de pouvoir comparer différentes dates pour effectuer des actions spécifiques, comme allumer une lumière à une heure précise ou déclencher une alarme à une certaine date. Dans cet article, nous allons explorer comment comparer deux dates en utilisant Arduino.

## Comment faire

Pour comparer deux dates en utilisant Arduino, nous allons devoir utiliser la bibliothèque `Time`, qui nous permet de gérer les dates et les heures. Tout d'abord, nous allons déclarer deux variables `time1` et `time2` afin de stocker les deux dates à comparer.

```arduino
#include <Time.h>
...
int time1 = time(2021, 10, 1, 12, 45, 30);
int time2 = time(2021, 10, 1, 12, 46, 30);
```

Ensuite, nous allons utiliser une fonction `difftime` pour calculer la différence entre les deux dates. Cette fonction renvoie le nombre de secondes entre les deux dates.

```arduino
int difference = difftime(time2, time1);
```

En utilisant cette valeur, nous pouvons alors effectuer des comparaisons pour déterminer si une date est avant, après ou égale à l'autre.

```arduino
if (difference > 0) {
  // time1 est avant time2
} else if (difference < 0) {
  // time1 est après time2
} else {
  // les deux dates sont identiques
}
```

Maintenant que nous savons comment comparer deux dates, nous pouvons utiliser cette connaissance dans nos projets Arduino pour effectuer des actions basées sur des horaires ou des dates spécifiques.

## Plongée en profondeur

Il est important de noter que les variables `time1` et `time2` doivent être utilisées avec la fonction `time()` pour stocker des dates et heures basées sur le temps réel. Sinon, ces variables peuvent être remplies avec des valeurs incorrectes.

De plus, la fonction `difftime` peut également être utilisée pour comparer des heures plutôt que des dates complètes. Dans ce cas, la fonction renverra le nombre de secondes entre les deux heures spécifiées.

## Voir aussi

- [Documentation de la bibliothèque `Time` pour Arduino](https://www.pjrc.com/teensy/td_libs_Time.html)
- [Tutorial sur la gestion des dates et heures en Arduino](https://randomnerdtutorials.com/arduino-ds1302-rtc-datetime-tutorial/) (en anglais)