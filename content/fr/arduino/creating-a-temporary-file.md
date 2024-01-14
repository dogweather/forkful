---
title:    "Arduino: Création d'un fichier temporaire"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

## Pourquoi

Vous êtes peut-être curieux de savoir pourquoi vous devriez créer un fichier temporaire lors de la programmation Arduino. La réponse est simple : les fichiers temporaires sont utilisés pour stocker des données temporaires qui sont nécessaires pour le bon fonctionnement de votre code.

## Comment faire

Pour créer un fichier temporaire dans votre code Arduino, vous pouvez suivre ces étapes simples :

- Utilisez la fonction `File tempFile = SD.open("FILE.txt", FILE_WRITE)`, en remplaçant `FILE` par le nom de votre fichier temporaire
- Écrivez vos données dans le fichier à l'aide de la fonction `tempFile.write()`
- Fermez le fichier temporaire à l'aide de la fonction `tempFile.close()`

Voici un exemple de code avec une fonction simple qui crée un fichier temporaire et y écrit une chaîne de caractères :

```Arduino
#include <SD.h>

void setup() {
  // Initialisez la communication avec la carte SD
  SD.begin(10);

  // Créer et ouvrir le fichier temporaire FILE.txt
  File tempFile = SD.open("FILE.txt", FILE_WRITE);

  // Écrire une chaîne de caractères dans le fichier temporaire
  tempFile.write("Ceci est un fichier temporaire.");

  // Fermer le fichier temporaire
  tempFile.close();
}

void loop() {
  // Votre code ici
}
```

Lorsque vous téléversez ce code sur votre carte Arduino, un fichier temporaire nommé FILE.txt sera créé et contiendra la chaîne de caractères que vous avez écrite.

## Approfondissement

Maintenant que vous savez comment créer un fichier temporaire dans votre code Arduino, voici quelques informations supplémentaires sur leur utilisation :

- Les fichiers temporaires sont automatiquement supprimés lorsque vous éteignez votre Arduino ou lorsque vous débranchez votre carte SD, il n'est donc pas nécessaire de les supprimer manuellement.
- Vous pouvez également utiliser des fichiers temporaires pour stocker des variables ou des données d'initialisation pour votre code, afin de les lire et de les utiliser à chaque exécution de votre programme.

## Voir aussi

Maintenant que vous en savez plus sur la création de fichiers temporaires dans votre code Arduino, voici quelques liens utiles pour approfondir vos connaissances :

- [Documentation officielle sur les fichiers temporaires avec Arduino](https://www.arduino.cc/en/Reference/SDOpen)
- [Exemple de projet Arduino utilisant des fichiers temporaires](https://create.arduino.cc/projecthub/arduino/fat16-how-to-store-files-in-an-arduino-5af1c0)
- [Tutoriel sur l'utilisation de la carte SD avec Arduino](https://www.circuitbasics.com/arduino-data-logger-tutorial/)