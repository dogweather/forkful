---
date: 2024-01-20 17:53:40.510256-07:00
description: 'How to (Comment faire) : Pour lire un fichier texte sur Arduino, il
  faut souvent utiliser une carte SD avec le module SD.h. Voici comment s''y prendre
  .'
lastmod: '2024-03-13T22:44:58.132636-06:00'
model: gpt-4-1106-preview
summary: Pour lire un fichier texte sur Arduino, il faut souvent utiliser une carte
  SD avec le module SD.h.
title: Lecture d'un fichier texte
weight: 22
---

## How to (Comment faire) :
Pour lire un fichier texte sur Arduino, il faut souvent utiliser une carte SD avec le module SD.h. Voici comment s'y prendre :

```Arduino
#include <SD.h>
#include <SPI.h>

void setup() {
  Serial.begin(9600);

  // Vérifiez que la carte SD est bien présente
  if (!SD.begin(4)) {
    Serial.println("Échec d'initialisation de la carte SD");
    return;
  }

  File fichier = SD.open("exemple.txt");
  
  // Lit le fichier si celui-ci est ouvert
  if (fichier) {
    while (fichier.available()) {
      Serial.write(fichier.read());
    }
    fichier.close(); // Toujours fermer le fichier après usage
  } else {
    // Si le fichier ne s'ouvre pas, imprimez un message d'erreur
    Serial.println("Erreur d'ouverture du fichier");
  }
}

void loop() {
  // Pas de boucle ici
}
```
Résultat attendu :

```
Contenu du fichier texte affiché ici.
```

## Deep Dive (Plongée en profondeur)
Historiquement, la lecture de fichiers sur des microcontrôleurs était compliquée à cause des limites de mémoire et de stockage. Avec l’avènement de modules externes comme les cartes SD, cela est devenu plus accessible. Alternative aux cartes SD, l’EEPROM sur Arduino permet de stocker des données mais elle est plus limitée en taille. Pour la lecture de fichiers, il faut jongler avec les ressources en mémoire pour ne pas saturer l'Arduino, surtout pour les gros fichiers. Cela peut impliquer de lire par blocs et de traiter ces blocs ligne par ligne.

## See Also (Voir aussi)
Pour plus d'infos sur le stockage de données avec Arduino, consultez les liens suivants :
- Documentation sur le module SD.h : https://www.arduino.cc/en/reference/SD
- Tutoriel pour la lecture et écriture de fichiers : https://www.arduino.cc/en/Tutorial/LibraryExamples/ReadWrite
- Utilisation de l’EEPROM sur Arduino : https://www.arduino.cc/en/Tutorial/EEPROMReadWrite
