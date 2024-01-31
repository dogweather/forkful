---
title:                "Écriture d'un fichier texte"
date:                  2024-01-19
simple_title:         "Écriture d'un fichier texte"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/arduino/writing-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why? / Quoi et Pourquoi?
Écrire dans un fichier texte permet de sauvegarder des données. Les programmeurs font ça pour conserver des infos comme des logs, des paramètres ou des données sensorielles.

## How to: / Comment faire :
```arduino
#include <SD.h>

File monFichier;

void setup() {
  Serial.begin(9600);
  if (!SD.begin(4)) {
    Serial.println("Initialisation SD échouée!");
    return;
  }
  monFichier = SD.open("test.txt", FILE_WRITE);

  if (monFichier) {
    monFichier.println("Salut Arduino!");
    monFichier.close(); 
    Serial.println("Écriture réalisée");
  } else {
    Serial.println("Erreur d'ouverture du fichier");
  }
}

void loop() {
  // Pas besoin de code ici pour l'instant.
}
```

## Deep Dive / Plongée en Profondeur
Créer ou ouvrir et écrire dans des fichiers texte avec Arduino a commencé avec les cartes SD. D'autres options incluent l'EEPROM ou les systèmes de fichiers SPIFFS/NFFS sur ESP. Ces techniques dépendent de la bibliothèque et du matériel utilisé. Écrire efficacement minimise l'usure de la mémoire.

## See Also / Voir Aussi
- Arduino SD library - https://www.arduino.cc/en/Reference/SD
- Guide to PROGMEM on Arduino - https://www.arduino.cc/reference/en/language/variables/utilities/progmem/
- File system usage with ESP8266/ESP32 - https://docs.espressif.com/projects/esp-idf/en/latest/esp32/api-reference/storage/spiffs.html
