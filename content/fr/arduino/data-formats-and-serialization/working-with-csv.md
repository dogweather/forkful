---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:18:44.285680-07:00
description: "Comment faire : Arduino n'a pas de biblioth\xE8que int\xE9gr\xE9e sp\xE9\
  cifiquement pour la gestion des fichiers CSV, mais vous pouvez utiliser les biblioth\xE8\
  ques `SD`\u2026"
lastmod: '2024-03-13T22:44:58.139110-06:00'
model: gpt-4-0125-preview
summary: "Arduino n'a pas de biblioth\xE8que int\xE9gr\xE9e sp\xE9cifiquement pour\
  \ la gestion des fichiers CSV, mais vous pouvez utiliser les biblioth\xE8ques `SD`\
  \ et `SPI` pour acc\xE9der aux fichiers sur une carte SD, puis analyser ou g\xE9\
  n\xE9rer des donn\xE9es CSV en utilisant des techniques simples de manipulation\
  \ de cha\xEEnes de caract\xE8res."
title: Travailler avec CSV
weight: 37
---

## Comment faire :
Arduino n'a pas de bibliothèque intégrée spécifiquement pour la gestion des fichiers CSV, mais vous pouvez utiliser les bibliothèques `SD` et `SPI` pour accéder aux fichiers sur une carte SD, puis analyser ou générer des données CSV en utilisant des techniques simples de manipulation de chaînes de caractères. Lors de la manipulation de CSV plus complexes, la bibliothèque tierce `ArduinoCSV` peut être utilisée pour faciliter l'analyse et l'écriture.

**Lecture des données CSV depuis une carte SD :**
```cpp
#include <SPI.h>
#include <SD.h>

void setup() {
  Serial.begin(9600);
  if (!SD.begin(4)) {
    Serial.println("L'initialisation a échoué !");
    return;
  }
  File dataFile = SD.open("data.csv");
  if (dataFile) {
    while (dataFile.available()) {
      String dataLine = dataFile.readStringUntil('\n');
      Serial.println(dataLine); // Affiche la ligne CSV
    }
    dataFile.close();
  } else {
    Serial.println("Erreur lors de l'ouverture de data.csv");
  }
}

void loop() {
  // Non utilisé dans cet exemple
}
```
*Exemple de sortie :*
```
SensorID, Timestamp, Value
1, 1597840923, 23.5
2, 1597840987, 22.4
```

**Écriture des données CSV sur une carte SD :**
```cpp
#include <SPI.h>
#include <SD.h>

void setup() {
  Serial.begin(9600);
  if (!SD.begin(4)) {
    Serial.println("L'initialisation a échoué !");
    return;
  }
  File dataFile = SD.open("output.csv", FILE_WRITE);
  if (dataFile) {
    dataFile.println("SensorID, Timestamp, Value"); // En-tête CSV
    dataFile.println("1, 1597840923, 23.5"); // Ligne de données exemple
    dataFile.close();
    Serial.println("Données écrites");
  } else {
    Serial.println("Erreur lors de l'ouverture de output.csv");
  }
}

void loop() {
  // Non utilisé dans cet exemple
}
```
*Exemple de sortie :*
```
Données écrites
```

**Utilisation d'ArduinoCSV pour l'analyse :**
Si vous gérez des fichiers CSV complexes, la bibliothèque `ArduinoCSV` peut considérablement simplifier les efforts d'analyse. Cet exemple suppose que vous avez déjà installé la bibliothèque `ArduinoCSV`.

```cpp
#include <SPI.h>
#include <SD.h>
#include <ArduinoCSV.h>

void setup() {
  Serial.begin(9600);
  if (!SD.begin(4)) {
    Serial.println("L'initialisation a échoué !");
    return;
  }
  File dataFile = SD.open("data.csv");
  if (dataFile) {
    CSVParser parser;
    while (dataFile.available()) {
      String dataLine = dataFile.readStringUntil('\n');
      if (parser.parseLine(dataLine)) {
        for (int i = 0; i < parser.count(); i++) {
          Serial.print(parser.getField(i)); // Imprime chaque champ
          if (i < parser.count() - 1) {
            Serial.print(", ");
          }
        }
        Serial.println();
      }
    }
    dataFile.close();
  } else {
    Serial.println("Erreur lors de l'ouverture de data.csv");
  }
}

void loop() {
  // Non utilisé dans cet exemple
}
```
*Exemple de sortie :*
```
SensorID,  Timestamp,  Value
1,  1597840923,  23.5
2,  1597840987,  22.4
```
Dans ces exemples, en lisant et en écrivant dans des fichiers CSV sur une carte SD, les projets Arduino peuvent facilement collecter des données, stocker des configurations ou échanger des données avec d'autres applications dans un format universellement accessible.
