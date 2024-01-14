---
title:                "Arduino: Créer un fichier temporaire"
programming_language: "Arduino"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/arduino/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous êtes passionné par la programmation et que vous aimez apprendre de nouvelles compétences, alors vous êtes au bon endroit ! Aujourd'hui, nous allons parler de la création de fichiers temporaires en utilisant Arduino.

## Comment faire

La création d'un fichier temporaire peut être utile dans de nombreuses situations, comme la sauvegarde de données temporaires ou l'utilisation de fonctions de débogage. Avec Arduino, il est facile de créer un fichier temporaire en utilisant la bibliothèque SD.

```Arduino
#include <SPI.h>
#include <SD.h>

File dataFile; // Création de l'objet de fichier

void setup() {
  Serial.begin(9600); // Initialisation de la communication série
  while (!Serial) {
    ; // Attente de la connexion du moniteur série
  }

// Initialisation de la carte SD
  Serial.print("Initialisation de la carte SD...");
  if (!SD.begin(4)) {
    Serial.println("ÉCHEC !");
    return;
  }
  Serial.println("RÉUSSITE !");
}

void loop() {
  // Ouverture du fichier temporaire
  dataFile = SD.open("temp.txt", FILE_WRITE);
  if (dataFile) {
    Serial.println("Écriture dans le fichier temporaire...");
    dataFile.println("Contenu temporaire"); // Écriture dans le fichier
    dataFile.close(); // Fermeture du fichier
    Serial.println("ÉCRITURE RÉUSSITE !");
    
  // Lecture du fichier pour vérifier le contenu
    dataFile = SD.open("temp.txt");
    
    while (dataFile.available()) {
      Serial.write(dataFile.read());
    }
    dataFile.close();
    
  // Suppression du fichier temporaire
    if (SD.remove("temp.txt")) {
      Serial.println("Fichier temporaire supprimé.");
    } else {
      Serial.println("Échec de la suppression du fichier temporaire.");
    }
    delay(1000); // Attente d'une seconde avant de recommencer
  }
}

```
Lorsque vous téléversez ce code sur votre carte Arduino ayant un module SD, vous verrez que le contenu du fichier temporaire sera affiché sur votre moniteur série avant qu'il ne soit supprimé.

## Plongeon en profondeur

Maintenant, jetons un coup d'œil à la ligne importante de code dans ce programme : `dataFile = SD.open("temp.txt", FILE_WRITE);`. Cette ligne ouvre le fichier temporaire avec le nom "temp.txt" en mode écriture. Le mode d'écriture permet d'écrire du contenu dans le fichier tandis que le mode lecture permet de lire le contenu du fichier.

De plus, il est important de fermer le fichier après avoir terminé d'écrire ou de lire. Vous pouvez également utiliser la fonction `SD.remove()` pour supprimer le fichier temporaire après utilisation.

Maintenant que vous comprenez comment créer un fichier temporaire en utilisant Arduino, vous pouvez l'implémenter dans vos futurs projets !

## Voir aussi

Si vous souhaitez en savoir plus sur la création de fichiers temporaires en utilisant Arduino, vous pouvez consulter les ressources suivantes :

- [Tutoriel sur l'utilisation de la bibliothèque SD avec Arduino](https://www.arduino.cc/en/Reference/SD)
- [Exemples de projets utilisant des fichiers temporaires avec Arduino](https://create.arduino.cc/projecthub/search?q=temporary+file)