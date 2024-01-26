---
title:                "Vérification de l'existence d'un répertoire"
html_title:           "Bash: Vérification de l'existence d'un répertoire"
simple_title:         "Vérification de l'existence d'un répertoire"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/arduino/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Quoi et Pourquoi ?

Vérifier l'existence d'un répertoire c'est s'assurer qu'un dossier spécifique est présent sur la carte SD de notre Arduino. On fait ça pour éviter des erreurs lors de la création, la lecture ou l'écriture de fichiers - personne n'aime un code qui plante parce qu'il ne trouve pas son chemin.

## Comment faire :

```Arduino
#include <SD.h>

void setup() {
  Serial.begin(9600);
  if (!SD.begin()) {
    Serial.println("Erreur d'initialisation de la carte SD");
    return;
  }
  if (SD.exists("/monDossier")) {
    Serial.println("Le dossier existe !");
  } else {
    Serial.println("Le dossier n'existe pas.");
  }
}

void loop() {
  // rien ici
}
```

Output:
```
Le dossier existe !
```
ou
```
Le dossier n'existe pas.
```

## Plongée profonde

Avant l'existence des cartes SD sur Arduino, on stockait les données de manière rudimentaire : EEPROM ou mémoires externes complexes. Avec l'intégration de la librairie SD, vérifier un répertoire est devenu un jeu d'enfant. En termes d'alternatives, d'autres librairies comme SPIFFS pour ESP8266 ou ESP32 gèrent aussi cette fonctionnalité mais différemment. Pour ce qui est des détails d'implémentation, `SD.exists()` s'appuie sur le système de fichiers FAT (File Allocation Table) et recherche l'entrée correspondante à votre dossier.

## Voir aussi

- Documentation Arduino sur `SD.exists()`: https://www.arduino.cc/en/Reference/SDExists
- La librairie SD sur GitHub : https://github.com/arduino-libraries/SD
- Tutorial Arduino SD Card : https://www.arduino.cc/en/Tutorial/LibraryExamples/CardInfo
