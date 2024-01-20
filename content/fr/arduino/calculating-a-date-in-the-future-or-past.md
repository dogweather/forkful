---
title:                "Calculer une date dans le futur ou le passé"
html_title:           "Arduino: Calculer une date dans le futur ou le passé"
simple_title:         "Calculer une date dans le futur ou le passé"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/arduino/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi?
Calculer une date dans le futur ou le passé consiste à ajouter ou soustraire un certain nombre de jours à une date spécifique. Les programmeurs le font souvent pour gérer les événements planifiés et les rappels dans leurs applications.

## Comment faire:
Arduino ne dispose pas d’une bibliothèque intégrée pour gérer les dates comme certains autres langages. On peut cependant utiliser la bibliothèque Time pour gérer le temps et les dates. Voyons comment ajouter des jours à une date.

```Arduino
#include <TimeLib.h>

void setup() {
    Serial.begin(9600);
    setTime(16, 40, 0, 4, 1, 2021); //16:40:00  on 04 Jan 2021
}

void loop() {
    time_t t = now();
    t += 10 * SECS_PER_DAY; //Ajouter dix jours
    Serial.println(day(t)); //Jour après 10 jours
    Serial.println(month(t)); //Mois après 10 jours
    Serial.println(year(t)); // Année après 10 jours
    delay(5000);
}
```

Dans cet exemple, nous ajoutons dix jours à la date du 4 janvier 2021.

## Explication approfondie
Historiquement, le calcul des dates dans Arduino n'était pas aussi simple que dans des environnements comme Python ou Java, qui ont des bibliothèques détaillées pour gérer les dates. En revanche, Arduino a une bibliothèque Time, mais ses fonctionnalités sont plus limitées.

Parmi les alternatives, on peut citer la bibliothèque DS3231, qui gère le temps réel et les dates en utilisant un module RTC externe. Elle est plus précise mais nécessite du matériel supplémentaire.

Sur le plan de l'implémentation, la bibliothèque Time stocke le temps en secondes depuis l'époque UNIX (1er janvier 1970), ce qui simplifie les calculs. Lorsque vous ajoutez ou soustrayez des jours, vous convertissez simplement ces jours en secondes et effectuez votre calcul.

## Voir également
Pour plus de détails sur l'utilisation de la bibliothèque Time, consultez https://www.pjrc.com/teensy/td_libs_Time.html.

Pour en savoir plus sur la bibliothèque DS3231, visitez https://github.com/adafruit/RTClib. 

Notez que ces bibliothèques ne prennent pas en compte les changements d'heure dus à l'heure d'été. Pour cela, vous pourriez envisager une bibliothèque comme Timezone, disponible ici: https://github.com/JChristensen/Timezone.