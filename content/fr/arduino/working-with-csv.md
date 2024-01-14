---
title:                "Arduino: Travailler avec des fichiers csv"
simple_title:         "Travailler avec des fichiers csv"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/arduino/working-with-csv.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous êtes passionné par la programmation et les projets électroniques, travailler avec des fichiers CSV peut être un excellent moyen d'améliorer vos compétences. Les fichiers CSV sont un moyen pratique de stocker et de manipuler des données tabulaires, ce qui peut être particulièrement utile pour les projets impliquant des capteurs et des actionneurs.

## Comment faire

Pour travailler avec des fichiers CSV sur Arduino, vous aurez besoin d'une carte compatible avec une mémoire suffisamment grande pour stocker le fichier. Vous devrez également inclure la bibliothèque SD pour accéder à la carte SD.

Une fois que vous avez ces éléments en place, le processus général pour travailler avec des fichiers CSV sur Arduino est le suivant:

1. Ouvrez le fichier CSV en utilisant la méthode `file.open()` et spécifiez le mode "lecture" (`FILE_READ`).
2. Parcourez le fichier en utilisant la méthode `file.available()` pour vérifier qu'il y a des données à lire, puis utilisez `file.parseInt()` pour lire chaque valeur entière.
3. Stockez les valeurs lues dans des variables ou effectuez des actions en fonction des valeurs.

Voici un exemple de code qui lit et imprime les données d'un fichier CSV avec une colonne de température et une colonne d'humidité:

```
#include <SPI.h>
#include <SD.h>

// Spécifie le nombre de lignes à sauter avant de commencer à lire les données réelles
#define HEADER_ROWS 1
// Spécifie les pins à utiliser pour la carte SD
#define SD_CS_PIN 4
#define SD_MOSI_PIN 11
#define SD_MISO_PIN 12
#define SD_SCK_PIN 13

// Initialise la variable pour le fichier
File dataFile;

void setup() {
  // Initialise la communication avec la carte SD
  SD.begin(SD_CS_PIN, SD_MOSI_PIN, SD_MISO_PIN, SD_SCK_PIN);
  
  // Ouvre le fichier CSV en mode lecture
  dataFile = SD.open("data.csv", FILE_READ);

  // Saute les lignes d'en-tête à partir du fichier
  for (int i = 0; i < HEADER_ROWS; i++) {
    dataFile.readStringUntil('\n');
  }
}

void loop() {
  // Vérifie qu'il y a des données à lire dans le fichier
  if (dataFile.available()) {
    // Lit la température en tant que nombre entier et stocke dans la variable "temp"
    int temp = dataFile.parseInt();
    // Lit l'humidité en tant que nombre entier et stocke dans la variable "hum"
    int hum = dataFile.parseInt();

    // Imprime les valeurs lues
    Serial.print("Température: ");
    Serial.print(temp);
    Serial.print("°C, Humidité: ");
    Serial.print(hum);
    Serial.println("%");

    // Passe à la ligne suivante du fichier
    dataFile.readStringUntil('\n');
  }
}
```

En utilisant ce code comme point de départ, vous pouvez également utiliser d'autres méthodes de la bibliothèque SD pour lire et écrire des données dans des fichiers CSV en fonction de vos besoins.

## Plongée profonde

Si vous souhaitez aller plus loin avec les fichiers CSV sur Arduino, vous pouvez également explorer d'autres bibliothèques tierces qui offrent des fonctionnalités plus avancées telles que la création de nouveaux fichiers CSV, la modification de données existantes ou même l'utilisation de fichiers CSV pour stocker des paramètres pour votre projet.

De plus, il peut être utile de comprendre le format de fichiers CSV et les contraintes telles que les limites de taille des fichiers que votre carte Arduino peut supporter.

## Voir aussi

- [Guide ultime pour travailler avec des fichiers CSV sur Arduino (en anglais)](https://learn.sparkfun.com/tutorials/using-the-arduino-pro-mini-33v?_ga=2.211830815.502129788.1622354909-1034997019.1622354909#hf-CodingYourCircuit)
- [Documentation officielle sur la bibliothèque SD pour Arduino](https://www.arduino.cc/en/Reference/SD)
- [Tutoriel sur l'utilisation de fichiers CSV pour stocker des paramètres sur Arduino (en anglais)](https://makezine.com/2011/03/15/arduino-basics-saving-data-to-a-sd/)