---
title:                "Arduino: Obtenir la date actuelle"
programming_language: "Arduino"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/arduino/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Pourquoi

Vous vous demandez peut-être pourquoi il serait utile d'afficher la date actuelle sur votre Arduino. Eh bien, il existe de nombreuses raisons pour lesquelles cela pourrait être utile. Par exemple, vous pourriez vouloir afficher la date sur un écran LCD pour suivre la durée de vie de votre projet ou pour l'utiliser dans une fonctionnalité avec une programmation avancée. Quelle que soit la raison, il est toujours bon d'apprendre à obtenir la date actuelle sur votre Arduino.

## Comment faire

Pour obtenir la date actuelle sur votre Arduino, vous aurez besoin du module de temps (RTC) DS3231. Il vous faudra également une bibliothèque appelée "DS3231.h" pour communiquer avec le module RTC. Si vous utilisez l'IDE Arduino, vous pouvez facilement ajouter cette bibliothèque en allant dans "Sketch" puis "Inclure une bibliothèque" et en sélectionnant "Gérer les bibliothèques". Recherchez "DS3231" et installez la bibliothèque.

Une fois la bibliothèque installée, vous devrez connecter votre module RTC à votre Arduino comme suit :

- Broche Vcc du module RTC à la broche 5V de l'Arduino
- Broche GND du module RTC à la broche GND de l'Arduino
- Broche SDA du module RTC à la broche SDA de l'Arduino
- Broche SCL du module RTC à la broche SCL de l'Arduino

Ensuite, dans votre code, vous devrez inclure la bibliothèque "DS3231.h" et créer un objet de type DS3231 en utilisant la syntaxe suivante :

```Arduino
#include <DS3231.h>

DS3231 rtc;
```

Ensuite, pour obtenir la date actuelle, vous pouvez utiliser les fonctions suivantes :

- rtc.getDayOfWeek() : renvoie le jour de la semaine en tant que nombre (1 pour dimanche, 2 pour lundi, etc.)
- rtc.getDateStr() : renvoie la date au format "mm/dd/yyyy"
- rtc.getTimeStr() : renvoie l'heure au format "hh:mm:ss"

Vous pouvez utiliser ces fonctions pour afficher la date et l'heure sur votre projet Arduino comme vous le souhaitez. Par exemple, vous pouvez les imprimer sur le moniteur série ou les afficher sur un écran LCD.

## Plongée en profondeur

Maintenant que vous savez comment obtenir la date actuelle sur votre Arduino, vous pourriez vous demander comment cela fonctionne en interne. Lorsque vous utilisez un module RTC, vous ne dépendez pas du temps de votre ordinateur, mais du temps du module lui-même. Le module RTC contient un oscillateur interne qui est utilisé pour générer des signaux d'horloge précis. Cela permet au module de suivre le temps même si votre Arduino est déconnecté de l'alimentation.

De plus, le module RTC utilise une batterie sauvegardée pour maintenir l'heure et la date même lorsque le module n'est pas alimenté. Cela garantit que votre Arduino affiche toujours la date correcte même après une coupure d'alimentation.

## Voir aussi

- Tutoriel sur l'utilisation du module RTC DS3231 : https://www.ladyada.net/learn/breakoutplush3/rtc.html
- Exemple de projet avec affichage de la date et de l'heure sur un écran LCD : https://create.arduino.cc/projecthub/Arduino_Genuino/lcd-real-time-clock-rtc-with-ds3231-module-a4fc93
- Documentation complète de la bibliothèque "DS3231.h" : https://github.com/jarzebski/Arduino-DS3231/blob/master/DS3231.h