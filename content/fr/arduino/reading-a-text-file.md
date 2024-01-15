---
title:                "Lecture d'un fichier texte"
html_title:           "Arduino: Lecture d'un fichier texte"
simple_title:         "Lecture d'un fichier texte"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/arduino/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous utilisez un Arduino, il est possible que vous ayez besoin de lire des fichiers texte pour stocker ou récupérer des données. Dans cet article, nous allons vous expliquer comment lire un fichier texte avec votre Arduino.

## Comment faire

Pour lire un fichier texte avec votre Arduino, vous devez suivre ces étapes simples :

- Connectez votre Arduino à votre ordinateur via un câble USB.
- Dans l'IDE Arduino, ouvrez la fenêtre "Exemples" (Examples) et choisissez "SD", puis "ReadAsciiFile" pour afficher un exemple de code pour lire des fichiers texte.

```
#include <SD.h>
File myFile;
void setup() {
  Serial.begin(9600);

  Serial.print("initializing SD card...");
  if (!SD.begin(4)) {
    Serial.println("initialization failed!");
    return;
  }
  Serial.println("initialization done.");

  myFile = SD.open("test.txt");

  if (myFile) {
    while (myFile.available()) {
      Serial.write(myFile.read());
    }
    myFile.close();
  } else {
    Serial.println("error opening test.txt");
  }
}

void loop() {
  // rien ne se passe ici
}
```

Dans cet exemple, nous utilisons la bibliothèque "SD" pour lire le fichier texte "test.txt". Tout d'abord, nous initialisons la carte SD et ouvrons le fichier avec la fonction `SD.open()`. Ensuite, nous lisons et affichons le contenu du fichier avec la boucle `while`. Enfin, nous fermons le fichier avec la fonction `myFile.close()`.

La sortie de ce code sera affichée dans la console série de l'IDE Arduino.

## Plongée en profondeur

Pour lire des fichiers textes avec votre Arduino, vous devez utiliser une carte SD. Vous pouvez soit utiliser une carte SD avec un adaptateur, soit utiliser un module SD spécialement conçu pour les projets Arduino.

De plus, si vous souhaitez écrire des données dans un fichier texte, vous pouvez utiliser la fonction `myFile.write()` pour ajouter des données à la fin du fichier existant ou la fonction `myFile.println()` pour créer une nouvelle ligne dans le fichier.

## Voir aussi

Pour plus d'informations sur la lecture de fichiers texte avec un Arduino, vous pouvez consulter les ressources suivantes :

- [Documentation officielle Arduino sur la lecture de fichiers sur une carte SD](https://www.arduino.cc/en/Reference/SD)
- [Tutoriel sur la lecture et l'écriture de fichiers sur une carte SD avec un Arduino](https://learn.adafruit.com/adafruit-micro-sd-breakout-board-card-tutorial/using-sd-vs-and-minisd)
- [Exemple de projet utilisant la lecture de fichiers sur une carte SD avec un Arduino](https://create.arduino.cc/projecthub/Rei_Vilo/read-write-a-string-in-a-sd-card-using-apdh0003-a1ea37?f=1)

Maintenant que vous savez comment lire des fichiers texte avec votre Arduino, vous pouvez utiliser cette fonctionnalité pour stocker et récupérer des données dans vos projets !