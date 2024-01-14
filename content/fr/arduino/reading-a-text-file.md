---
title:                "Arduino: Lecture d'un fichier texte"
programming_language: "Arduino"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/arduino/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Pourquoi

L'utilisation de fichiers texte est essentielle pour stocker et accéder à de grandes quantités de données dans vos projets Arduino. Que vous ayez besoin de stocker des données de capteurs, de créer des menus ou tout simplement de stocker du texte dans votre projet, la lecture de fichiers texte est une compétence importante à avoir.

## Comment

La première étape pour lire un fichier texte dans votre projet Arduino est de créer un objet File à l'aide de la fonction `SD.open()`. Cela vous permet d'ouvrir le fichier et d'y accéder pour la lecture. Ensuite, vous pouvez utiliser la fonction `read()` pour lire le contenu du fichier et le stocker dans une variable. Voici un exemple de code pour lire un fichier texte nommé "texte.txt" :

```Arduino
#include <SD.h>

File monFichier;

// Ouverture du fichier
monFichier = SD.open("texte.txt");

// Lecture du contenu du fichier
while (monFichier.available()) {
  char character = monFichier.read();
  // Faire quelque chose avec le contenu du fichier
}

// Fermeture du fichier
monFichier.close();
```

Cette méthode fonctionne pour les fichiers texte simples, mais que faire si vous avez besoin de lire des fichiers avec une structure plus complexe, comme un CSV ou un JSON ? Pour cela, nous vous recommandons d'utiliser des bibliothèques spécifiques telles que "CSV" ou "ArduinoJSON" pour faciliter la lecture de ces types de fichiers. Vous pouvez trouver des exemples de code sur les sites respectifs de ces bibliothèques.

## Plongée en profondeur

Lors de la lecture de fichiers texte, il est important de comprendre comment les données sont stockées et comment les lire correctement. Les fichiers texte sont des séquences de caractères, donc lors de la lecture de ces fichiers, vous devez prendre en compte les sauts de ligne et les caractères spéciaux. De plus, assurez-vous d'utiliser la bonne méthode de lecture en fonction du type de données stockées dans votre fichier.

## Voir aussi

- [Documentation officielle sur la lecture de fichiers texte avec Arduino](https://www.arduino.cc/en/Reference/SDopen)
- [Bibliothèque CSV pour la lecture de fichiers CSV](https://github.com/rodan/davidz) 
- [Bibliothèque ArduinoJSON pour la lecture de fichiers JSON] (https://arduinojson.org/)

En utilisant ces informations et en explorant différentes bibliothèques, vous serez en mesure de lire et de traiter des fichiers texte dans vos projets Arduino avec facilité. Alors n'hésitez pas à utiliser cette compétence pour améliorer vos projets et leur ajouter une touche de complexité !