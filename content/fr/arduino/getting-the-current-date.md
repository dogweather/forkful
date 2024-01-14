---
title:                "Arduino: Obtenir la date actuelle"
simple_title:         "Obtenir la date actuelle"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/arduino/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Pourquoi

Vous êtes peut-être en train de vous demander pourquoi avoir la date actuelle serait important dans votre projet Arduino. Eh bien, permettez-moi de vous dire que c'est une fonctionnalité très utile dans de nombreux cas. Par exemple, vous pourriez avoir besoin de savoir quel jour il est pour un système d'arrosage automatique ou pour enregistrer la date et l'heure d'un événement dans un journal.

## Comment faire

La bonne nouvelle est qu'il est très facile de récupérer la date actuelle en utilisant Arduino. Tout d'abord, vous devez inclure la bibliothèque Time.h dans votre code en utilisant l'instruction `#include <Time.h>`. Ensuite, vous devez initialiser la bibliothèque en appelant la fonction `setTime()` avec les valeurs de l'heure et de la date actuelles. Par exemple :

```
Arduino setTime(14, 30, 0, 20, 3, 2021); // Définit l'heure à 14:30, le jour à 20 et le mois à mars en 2021
```

Ensuite, vous pouvez utiliser la fonction `now()` pour obtenir un objet Time qui contient toutes les informations sur la date et l'heure actuelles. Vous pouvez ensuite utiliser les fonctions `hour()`, `minute()`, `second()`, `day()`, `month()` et `year()` pour obtenir des valeurs spécifiques.

```
Arduino now = now(); // Stocke la date et l'heure actuelles dans l'objet Time
int heure = hour(now); // Stocke l'heure actuelle dans la variable heure
```

## Plongée en profondeur

Time.h utilise l'horloge interne de l'Arduino pour suivre l'heure et la date, donc il est important de s'assurer que l'heure et la date sont correctement définies avant d'utiliser cette bibliothèque. Vous pouvez le faire en utilisant un module RTC (Real Time Clock) ou en utilisant un code pour ajuster manuellement l'heure et la date. De plus, vous pouvez utiliser des fonctions telles que `dayOfWeek()` et `dayOfYear()` pour obtenir des informations supplémentaires sur la date actuelle.

## Voir aussi

- [Documentation officielle de la bibliothèque Time.h](https://www.arduino.cc/reference/en/libraries/time/)
- [Tutoriel vidéo sur la récupération de la date et de l'heure avec Arduino](https://www.youtube.com/watch?v=1JJhVviPDT0)
- [Utilisation d'un module RTC avec Arduino](https://howtomechatronics.com/tutorials/arduino/arduino-ds3231-real-time-clock-tutorial/)