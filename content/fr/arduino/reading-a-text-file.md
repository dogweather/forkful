---
title:                "Arduino: Lecture d'un fichier texte"
simple_title:         "Lecture d'un fichier texte"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/arduino/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous êtes nouveau dans le monde de la programmation Arduino, vous pourriez être curieux de savoir comment lire un fichier texte en utilisant cette plateforme. Obtenir des données à partir d'un fichier texte peut être utile pour une variété de projets, comme le stockage de valeurs de capteurs ou la lecture de configurations préétablies pour votre système.

## Comment faire

Pour lire un fichier texte en utilisant Arduino, vous devrez suivre ces étapes :

1. Connectez votre Arduino à votre ordinateur via un câble USB.

2. Ouvrez l'IDE Arduino et créez une nouvelle esquisse.

3. Utilisez la fonction ```SD.begin()``` pour initialiser la carte SD et la fonction ```SD.open()``` pour ouvrir le fichier texte.

4. Utilisez la fonction ```readLine()``` pour lire chaque ligne du fichier et stocker la valeur dans une variable. N'oubliez pas de convertir la valeur en la bonne unité, si nécessaire.

5. Répétez cette étape pour chaque ligne de texte que vous souhaitez lire.

6. Enfin, fermez le fichier en utilisant la fonction ```SD.close()``` et affichez les données lues sur le moniteur série à l'aide de la fonction ```Serial.println()```.

Voici un exemple de code qui lit trois valeurs de température stockées dans un fichier "temp.txt" sur la carte SD :

```
#include <SPI.h>
#include <SD.h>

File configFile;
float temperature1;
float temperature2;
float temperature3;

void setup() {
  // initialise la communication série
  Serial.begin(9600);

  // initialise la carte SD
  SD.begin(10);

  // ouvre le fichier texte en lecture seule
  configFile = SD.open("temp.txt");

  // lit chaque ligne du fichier et stocke la valeur dans une variable
  temperature1 = configFile.readFloat();
  temperature2 = configFile.readFloat();
  temperature3 = configFile.readFloat();

  // ferme le fichier
  configFile.close();

  // affiche les valeurs sur le moniteur série
  Serial.print("Température 1 : ");
  Serial.println(temperature1);
  Serial.print("Température 2 : ");
  Serial.println(temperature2);
  Serial.print("Température 3 : ");
  Serial.println(temperature3);
}

void loop() {
  // le code ici ne sera pas exécuté car nous ne réalisons qu'une seule lecture au début
}
```

L'exemple de code ci-dessus suppose que les trois valeurs de température dans le fichier sont stockées en tant que valeurs de type ```float```. Vous devrez peut-être utiliser une fonction différente pour convertir la valeur dans le bon format.

## Plongée en profondeur

Il existe d'autres fonctions pour la lecture de fichiers texte sur la carte SD, telles que ```SD.exists()``` pour vérifier si un fichier existe et ```SD.remove()``` pour supprimer un fichier existant. Vous pouvez également utiliser la bibliothèque ```SD.h``` pour écrire des données dans un fichier texte.

De plus, il existe des bibliothèques tierces qui peuvent faciliter la lecture de fichiers texte, comme la bibliothèque ```SimpleSDAudio``` qui permet de lire des fichiers audio à partir de la carte SD.

Il est également important de noter que la lecture de fichiers texte à partir d'une carte SD peut être un processus relativement lent, en particulier si vous lisez de grandes quantités de données à chaque itération de la boucle. Il est donc important de prendre en compte le temps de lecture dans la conception de votre projet.

## Voir aussi

- [Tutoriel du projet Arduino officiel sur la lecture de fichiers texte](https://www.arduino.cc/en/Tutorial/DataReader)
- [Documentation officielle de la bibliothèque SD pour Arduino](https://www.arduino.cc/en/Reference/SD)
- [Bibliothèque SimpleSDAudio pour lire des fichiers audio à partir de la carte SD](https://github.com/earlephilhower/ESP8266Audio)