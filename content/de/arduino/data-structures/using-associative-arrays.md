---
changelog:
- 2024-01-30, gpt-4-0125-preview, translated from English
date: 2024-01-30 19:10:13.417490-07:00
description: "In der Welt von Arduino erm\xF6glichen assoziative Arrays, Schl\xFC\
  ssel mit Werten zu verkn\xFCpfen, \xE4hnlich wie man Socken ihren Paaren zuordnet.\
  \ Sie sind eine\u2026"
lastmod: '2024-03-13T22:44:54.137899-06:00'
model: gpt-4-0125-preview
summary: "In der Welt von Arduino erm\xF6glichen assoziative Arrays, Schl\xFCssel\
  \ mit Werten zu verkn\xFCpfen, \xE4hnlich wie man Socken ihren Paaren zuordnet."
title: Verwendung von assoziativen Arrays
weight: 15
---

## Was & Warum?
In der Welt von Arduino ermöglichen assoziative Arrays, Schlüssel mit Werten zu verknüpfen, ähnlich wie man Socken ihren Paaren zuordnet. Sie sind eine bevorzugte Wahl, wenn man Daten unter Verwendung beschreibender Namen speichern und abrufen möchte, was den Code sauberer und verständlicher macht.

## Wie:
Arduino bietet strikt genommen keine integrierte Unterstützung für assoziative Arrays, wie man sie in höheren Programmiersprachen findet. Aber keine Sorge. Wir können mit Strukturen und Arrays kreativ werden, um diese Funktionalität nachzuahmen. Hier ist ein einfaches Beispiel, um ein grundlegendes "assoziatives Array" zum Speichern und Zugreifen auf Temperaturen verschiedener Städte zu erstellen.

Definieren Sie zunächst eine Struktur, um die Stadt (Schlüssel) und ihre Temperatur (Wert) zu halten:

```cpp
struct CityTemperature {
  String city;
  float temperature;
};
```

Initialisieren Sie anschließend ein Array von `CityTemperature`-Objekten:

```cpp
CityTemperature temperatures[] = {
  {"New York", 19.5},
  {"Los Angeles", 22.0},
  {"Chicago", 17.0}
};
```

So können Sie auf die Temperatur einer bestimmten Stadt zugreifen und sie anzeigen:

```cpp
void setup() {
  Serial.begin(9600);
  for(int i = 0; i < 3; i++) {
    if(temperatures[i].city == "Los Angeles") {
      Serial.print("Die Temperatur in Los Angeles beträgt: ");
      Serial.println(temperatures[i].temperature);
    }
  }
}

void loop() {
  // Nichts hier für jetzt.
}
```

Die Ausführung dieses Codes würde folgende Ausgabe liefern:

```
Die Temperatur in Los Angeles beträgt: 22.0
```

## Tiefer Einblick
Historisch gesehen kamen Programmiersprachen wie C und C++ (von denen die Arduino-Syntax abgeleitet ist) nicht mit eingebauten assoziativen Arrays, was zu Umwegen wie dem oben gezeigten führte. Dieser Ansatz ist relativ einfach, skaliert jedoch schlecht, wenn die Datengröße aufgrund der O(n)-Suchzeit zunimmt.

Sprachen wie Python bieten Wörterbücher und JavaScript hat Objekte für diesen Zweck, die beide weit effizienter für die Verwaltung von Schlüssel-Wert-Paaren sind. In Arduino, wenn Leistung und Effizienz kritisch werden, könnten Entwickler für spezialisiertere Datenstrukturen wie Hashtabellen optieren, die über Bibliotheken implementiert werden.

Obwohl Arduino keine assoziativen Arrays nativ unterstützt, hat die Community Bibliotheken wie `HashMap` entwickelt, die Ihrem Projekt hinzugefügt werden können, um eine ähnliche Funktionalität mit besserer Leistung als ein DIY-Ansatz zu bieten. Diese Bibliotheken bieten in der Regel eine elegantere und effizientere Möglichkeit, assoziative Arrays zu verwalten, insbesondere für komplexere Projekte.
