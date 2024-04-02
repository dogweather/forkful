---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:13:24.616424-07:00
description: "P\xE4iv\xE4m\xE4\xE4r\xE4n j\xE4sent\xE4minen merkkijonosta Arduinolla\
  \ tarkoittaa p\xE4iv\xE4m\xE4\xE4r\xE4n osien (vuosi, kuukausi, p\xE4iv\xE4) poimimista\
  \ ja muuntamista tekstiesityksest\xE4\u2026"
lastmod: '2024-03-13T22:44:56.833034-06:00'
model: gpt-4-0125-preview
summary: "P\xE4iv\xE4m\xE4\xE4r\xE4n j\xE4sent\xE4minen merkkijonosta Arduinolla tarkoittaa\
  \ p\xE4iv\xE4m\xE4\xE4r\xE4n osien (vuosi, kuukausi, p\xE4iv\xE4) poimimista ja\
  \ muuntamista tekstiesityksest\xE4\u2026"
title: "P\xE4iv\xE4m\xE4\xE4r\xE4n j\xE4sennys merkkijonosta"
weight: 30
---

## Mikä & Miksi?

Päivämäärän jäsentäminen merkkijonosta Arduinolla tarkoittaa päivämäärän osien (vuosi, kuukausi, päivä) poimimista ja muuntamista tekstiesityksestä muotoon, jota voidaan käyttää ajanpidossa, vertailuissa tai manipuloinnissa luonnoksissa. Ohjelmoijat suorittavat usein tämän tehtävän käyttöliittymässä komponenttien, kuten reaaliaikakellojen, lokeroiden tai web-APIen ja käyttöliittymien syötteiden, kanssa, joissa päivämäärät saattavat olla esitettyinä luettavassa muodossa.

## Kuinka tehdä:

Suora lähestymistapa ilman kolmannen osapuolen kirjastoa:

```cpp
#include <Wire.h>
#include <RTClib.h>

void setup() {
  Serial.begin(9600);
  // Esimerkki päivämäärä merkkijonona YYYY-MM-DD muodossa
  String dateString = "2023-04-01"; 

  int year = dateString.substring(0, 4).toInt();
  int month = dateString.substring(5, 7).toInt();
  int day = dateString.substring(8, 10).toInt();

  // Alusta DateTime-objekti jäsenistä
  DateTime parsedDate(year, month, day);
  
  Serial.print("Jäsennetty Päivämäärä: ");
  Serial.print(parsedDate.year(), DEC);
  Serial.print("/");
  Serial.print(parsedDate.month(), DEC);
  Serial.print("/");
  Serial.println(parsedDate.day(), DEC);
}

void loop() {}
```

Esimerkki tuloste:
```
Jäsennetty Päivämäärä: 2023/4/1
```

Kolmannen osapuolen kirjaston käyttö (*ArduinoJson* monimutkaisempien jäsentämistilanteiden, kuten päivämäärän saamiseen JSON-vastauksesta, osalta):

Ensin, asenna ArduinoJson-kirjasto Arduinon Kirjastohallinnan kautta.

```cpp
#include <ArduinoJson.h>

void setup() {
  Serial.begin(9600);

  // Simuloi JSON-vastaus
  String jsonResponse = "{\"date\":\"2023-07-19\"}";
  StaticJsonDocument<200> doc;
  deserializeJson(doc, jsonResponse);

  // Poimi päivämäärä merkkijono
  const char* date = doc["date"];

  // Jäsennä päivämäärä merkkijonosta kuten aiemmin
  int year = String(date).substring(0, 4).toInt();
  int month = String(date).substring(5, 7).toInt();
  int day = String(date).substring(8, 10).toInt();
  
  Serial.print("Jäsennetty Päivämäärä JSONista: ");
  Serial.print(year);
  Serial.print("/");
  Serial.print(month);
  Serial.print("/");
  Serial.println(day);
}

void loop() {}
```

Esimerkki tuloste:
```
Jäsennetty Päivämäärä JSONista: 2023/7/19
```
