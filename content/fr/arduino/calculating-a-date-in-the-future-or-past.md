---
title:    "Arduino: Calculer une date dans le futur ou le passé"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/arduino/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Pourquoi

L'un des grands avantages de la programmation Arduino est la capacité de manipuler le temps et la date. Cela peut être utile pour une variété de projets tels que des systèmes de surveillance de l'eau ou des horloges automatiques. Dans cet article, nous allons explorer comment calculer une date dans le futur ou dans le passé en utilisant Arduino.

## Comment faire

Pour commencer, nous allons utiliser la bibliothèque "Time" disponible dans l'IDE Arduino pour faciliter la manipulation du temps et de la date. Pour calculer une date dans le futur ou dans le passé, nous allons utiliser la fonction "time_t" qui enregistre le temps en secondes depuis le 1er janvier 1970.

```Arduino
#include <TimeLib.h>

// Définir une date à l'aide des variables "year", "month", "day", "hour", "minute" et "second"
tmElements_t date;
date.Year = 2020;
date.Month = 10;
date.Day = 15;
date.Hour = 10;
date.Minute = 30;
date.Second = 0;

// Convertir la date en seconds depuis 1970
time_t date_in_seconds = makeTime(date);

// Ajouter ou soustraire le nombre de secondes à la date choisie
time_t date_future = date_in_seconds + 86400; //Ajouter une journée
time_t date_past = date_in_seconds - 3600; // Soustraire une heure

//Convertir en format lisible pour l'utilisateur
tmElements_t date_future_formatted;
tmElements_t date_past_formatted;
breakTime(date_future, date_future_formatted);
breakTime(date_past, date_past_formatted);

// Afficher la date dans le moniteur série
Serial.print("Date future : ");
Serial.println(date_future_formatted.Hour);
Serial.print("/");
Serial.print(date_future_formatted.Month);
Serial.print("/");
Serial.print(date_future_formatted.Year);

// Afficher la date dans le moniteur série
Serial.print("Date passé : ");
Serial.println(date_past_formatted.Hour);
Serial.print("/");
Serial.print(date_past_formatted.Month);
Serial.print("/");
Serial.print(date_past_formatted.Year);
```

La sortie du moniteur série affichera la date calculée dans le format choisi. Dans cet exemple, la date future sera un jour après la date initiale et la date passé sera une heure avant la date initiale.

## Plongée en profondeur

Il est possible d'utiliser cette méthode pour calculer des dates dans le futur ou dans le passé sur une longue période de temps. Cependant, il est important de prendre en compte les années bissextiles et les changements d'heure pour obtenir une date précise.

De plus, en utilisant la fonction "setTime" de la bibliothèque "Time", il est possible de régler l'horloge interne de l'Arduino à une date spécifique, ce qui peut être utile pour les projets nécessitant une synchronisation précise avec le temps réel.

## Voir aussi

- [Documentation de la bibliothèque Time pour Arduino](https://github.com/PaulStoffregen/Time)
- [Date et heure en temps réel avec Arduino](https://www.arduino.cc/en/Tutorial/BuiltInExamples/DateTime)
- [Utilisation de la bibliothèque Time dans les projets Arduino](https://howtomechatronics.com/tutorials/arduino/arduino-library-for-date-and-time/)

---

Merci d'avoir lu cet article sur comment calculer une date dans le futur ou dans le passé avec Arduino. Nous espérons que cela vous sera utile pour vos projets futurs ! N'hésitez pas à explorer d'autres fonctionnalités de la bibliothèque "Time" pour manipuler le temps et la date selon vos besoins. Bonne programmation !