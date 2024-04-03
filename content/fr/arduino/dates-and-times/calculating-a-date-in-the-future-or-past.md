---
date: 2024-01-20 17:30:43.788819-07:00
description: "Calculer une date dans le futur ou le pass\xE9 revient \xE0 trouver\
  \ une date en ajoutant ou soustrayant des jours \xE0 partir d'aujourd'hui. Les programmeurs\
  \ le\u2026"
lastmod: '2024-03-13T22:44:58.128289-06:00'
model: gpt-4-1106-preview
summary: "Calculer une date dans le futur ou le pass\xE9 revient \xE0 trouver une\
  \ date en ajoutant ou soustrayant des jours \xE0 partir d'aujourd'hui."
title: "Calcul d'une date future ou pass\xE9e"
weight: 26
---

## Quoi & Pourquoi ?
Calculer une date dans le futur ou le passé revient à trouver une date en ajoutant ou soustrayant des jours à partir d'aujourd'hui. Les programmeurs le font pour des rappels, des délais, et la planification d’événements.

## Comment faire :
Voici un exemple simple pour ajouter des jours à la date actuelle avec une carte Arduino et afficher le résultat sur le moniteur série.

```Arduino
#include <RTClib.h>

RTC_DS1307 rtc;

void setup() {
  Serial.begin(9600);
  if (!rtc.begin()) {
    Serial.println("RTC introuvable");
    while (1);
  }

  if (!rtc.isrunning()) {
    Serial.println("RTC n'est pas en cours d'exécution !");
    rtc.adjust(DateTime(__DATE__, __TIME__));
  }
}

void loop() {
  DateTime now = rtc.now();
  DateTime futureDate = now + TimeSpan(10, 0, 0, 0); // Ajouter 10 jours à la date actuelle

  Serial.print("Date dans 10 jours: ");
  Serial.print(futureDate.day());
  Serial.print("/");
  Serial.print(futureDate.month());
  Serial.print("/");
  Serial.println(futureDate.year());

  delay(10000); // Attendre 10 secondes avant de réafficher
}
```

Sortie d’exemple:
```
Date dans 10 jours: 22/3/2023
```

## Exploration approfondie
La gestion des dates a toujours été un sujet complexe en programmation à cause des différentes unités de temps et des exceptions comme les années bissextiles. L'Arduino utilise souvent des bibliothèques, comme `RTClib` pour les horloges en temps réel (RTC), pour manipuler des dates et des heures. D'autres méthodes incluent les calculs manuels ou l'utilisation des services d'heure réseau (NTP). L'implémentation préférera souvent les bibliothèques externes pour simplifier les calculs et prendre en compte les particularités du calendrier.

## Voir aussi
- Documentation de `RTClib` sur GitHub: [github.com/adafruit/RTClib](https://github.com/adafruit/RTClib)
- Guide Arduino sur les horloges en temps réel (RTC) : [www.arduino.cc/en/Guide/Libraries#rtclibraries](https://www.arduino.cc/en/Guide/Libraries#rtclibraries)
