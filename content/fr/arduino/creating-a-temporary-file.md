---
title:    "Arduino: Création d'un fichier temporaire"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/arduino/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Pourquoi

La création d'un fichier temporaire peut être utile dans de nombreuses situations lors de la programmation Arduino. Cela permet de stocker temporairement des données ou des informations, qui seront ensuite supprimées une fois qu'elles ne sont plus nécessaires. Cela peut également aider à économiser de l'espace de stockage et à optimiser les performances de votre projet.

## Comment faire

La création d'un fichier temporaire en utilisant Arduino est un processus simple en quelques étapes. Tout d'abord, vous devrez inclure la bibliothèque "SD.h" dans votre code. Ensuite, vous pouvez créer le fichier temporaire en utilisant la fonction "File.createTempFile()". Vous pouvez spécifier un nom de fichier et une extension facultative pour le fichier temporaire. Enfin, vous pouvez écrire des données dans le fichier temporaire en utilisant la fonction "file.println()" ou toute autre fonction d'écriture.

```Arduino
#include <SD.h>
File tempFile = File.createTempFile("mon_fichier_temporaire", ".txt");
tempFile.println("Ceci est un fichier temporaire créé avec Arduino!");
tempFile.close();
```
La sortie de cet exemple sera un fichier texte nommé "mon_fichier_temporaire.txt" avec le contenu "Ceci est un fichier temporaire créé avec Arduino!".

## Plongée en profondeur

Lors de la création d'un fichier temporaire avec Arduino, il est important de comprendre que le fichier sera stocké dans la mémoire SRAM de votre carte Arduino, et non sur une carte SD ou tout autre périphérique de stockage. Cela signifie que la taille du fichier temporaire sera limitée en fonction de la taille de la mémoire SRAM disponible. Il est donc important de veiller à ne pas créer de fichiers trop volumineux et de les supprimer une fois qu'ils ne sont plus nécessaires.

## Voir aussi

- [Documentation officielle sur la création de fichiers temporaires avec Arduino](https://www.arduino.cc/en/Reference/FileCreateTempFile)
- [Tutoriel sur l'utilisation de la bibliothèque SD pour stocker des données sur une carte SD avec Arduino](https://www.instructables.com/id/Save-Files-to-SD-Card-Using-Arduino/)
- [Article sur la gestion de la mémoire SRAM avec Arduino](http://gammon.com.au/millis)