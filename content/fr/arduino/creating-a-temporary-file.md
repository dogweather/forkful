---
title:                "Arduino: Création d'un fichier temporaire"
simple_title:         "Création d'un fichier temporaire"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/arduino/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Pourquoi

Créer un fichier temporaire est une pratique courante dans le développement d'Arduino. Cela permet de stocker temporairement des données ou des informations tout en libérant de l'espace mémoire pour d'autres tâches. Cela peut également être utile pour tester du code sans affecter directement les données permanentes.

## Comment faire

Pour créer un fichier temporaire dans Arduino, il suffit d'utiliser la bibliothèque SD et la fonction open (). Voici un exemple de code :

```Arduino
#include <SD.h>
File myFile;

void setup() {
  Serial.begin(9600);
  if(!SD.begin(4)) { // SD card connected to pin 4
    Serial.println("SD card failed to initialize");
    return;
  }
  
  myFile = SD.open("temp.txt", FILE_WRITE); // create a new file called temp.txt
  if (myFile) {
    myFile.println("This is a temporary file");
    myFile.close();
    Serial.println("Temporary file created");
  } else {
    Serial.println("Error creating temporary file");
  }
}

void loop() {
  // other code here
}
```
Le code ci-dessus crée un nouveau fichier appelé "temp.txt" et y écrit une phrase. Il vérifie également si le fichier a été créé avec succès et le mentionne dans le moniteur série.

## Plongée en profondeur

Lors de la création d'un fichier temporaire, il est important de choisir un nom unique afin d'éviter tout conflit avec d'autres fichiers. Vous pouvez également spécifier un chemin de fichier pour le stocker dans un dossier spécifique sur votre carte SD.

De plus, il est important de fermer correctement le fichier après avoir fini de l'utiliser, afin de libérer l'espace mémoire utilisé. Dans notre exemple, nous avons utilisé la fonction close () pour cela.

## Voir aussi

- [Documentation Arduino SD Library](https://www.arduino.cc/en/Reference/SD)
- [Guide pour utiliser une carte SD avec Arduino](https://learn.adafruit.com/adafruit-micro-sd-breakout-board-card-tutorial/overview)
- [Exemples de projets utilisant la bibliothèque SD](https://github.com/arduino-libraries/SD/tree/master/examples)