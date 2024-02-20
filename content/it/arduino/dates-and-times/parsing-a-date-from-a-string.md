---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:13:19.468012-07:00
description: "Effettuare il parsing di una data da una stringa in Arduino comporta\
  \ l'estrazione e la conversione delle componenti della data (anno, mese, giorno)\
  \ da una\u2026"
lastmod: 2024-02-19 22:05:02.768531
model: gpt-4-0125-preview
summary: "Effettuare il parsing di una data da una stringa in Arduino comporta l'estrazione\
  \ e la conversione delle componenti della data (anno, mese, giorno) da una\u2026"
title: Analisi di una data da una stringa
---

{{< edit_this_page >}}

## Cosa e Perché?

Effettuare il parsing di una data da una stringa in Arduino comporta l'estrazione e la conversione delle componenti della data (anno, mese, giorno) da una rappresentazione testuale a un formato che può essere utilizzato per la gestione del tempo, confronti o manipolazioni all'interno degli sketch. I programmatori eseguono frequentemente questo compito per interfacciarsi con componenti come orologi in tempo reale, registratori, o per elaborare input da API web e interfacce utente dove le date potrebbero essere presentate in un formato leggibile.

## Come fare:

Approccio diretto senza una libreria di terze parti:

```cpp
#include <Wire.h>
#include <RTClib.h>

void setup() {
  Serial.begin(9600);
  // Esempio di stringa data in formato AAAA-MM-GG
  String dateString = "2023-04-01"; 

  int year = dateString.substring(0, 4).toInt();
  int month = dateString.substring(5, 7).toInt();
  int day = dateString.substring(8, 10).toInt();

  // Inizializza un oggetto DateTime con le componenti analizzate
  DateTime parsedDate(year, month, day);
  
  Serial.print("Data Analizzata: ");
  Serial.print(parsedDate.year(), DEC);
  Serial.print("/");
  Serial.print(parsedDate.month(), DEC);
  Serial.print("/");
  Serial.println(parsedDate.day(), DEC);
}

void loop() {}
```

Output Esempio:
```
Data Analizzata: 2023/4/1
```

Utilizzando una libreria di terze parti (*ArduinoJson* per scenari di parsing più complessi, come l'ottenimento di una data da una risposta JSON):

Prima di tutto, installare la libreria ArduinoJson tramite il Gestore Librerie di Arduino.

```cpp
#include <ArduinoJson.h>

void setup() {
  Serial.begin(9600);

  // Simulazione di una risposta JSON
  String jsonResponse = "{\"date\":\"2023-07-19\"}";
  StaticJsonDocument<200> doc;
  deserializeJson(doc, jsonResponse);

  // Estrazione della stringa data
  const char* date = doc["date"];

  // Esegui il parsing della data dalla stringa come prima
  int year = String(date).substring(0, 4).toInt();
  int month = String(date).substring(5, 7).toInt();
  int day = String(date).substring(8, 10).toInt();
  
  Serial.print("Data Analizzata da JSON: ");
  Serial.print(year);
  Serial.print("/");
  Serial.print(month);
  Serial.print("/");
  Serial.println(day);
}

void loop() {}
```

Output Esempio:
```
Data Analizzata da JSON: 2023/7/19
```
