---
date: 2024-01-20 17:39:32.190917-07:00
description: "Cr\xE9er un fichier temporaire sur Arduino implique de stocker des donn\xE9\
  es de mani\xE8re transitoire pendant l'ex\xE9cution d'un sketch. On le fait souvent\
  \ pour\u2026"
lastmod: '2024-03-13T22:44:58.135163-06:00'
model: gpt-4-1106-preview
summary: "Cr\xE9er un fichier temporaire sur Arduino implique de stocker des donn\xE9\
  es de mani\xE8re transitoire pendant l'ex\xE9cution d'un sketch."
title: "Cr\xE9ation d'un fichier temporaire"
weight: 21
---

## Quoi & Pourquoi ?
Créer un fichier temporaire sur Arduino implique de stocker des données de manière transitoire pendant l'exécution d'un sketch. On le fait souvent pour gérer des données volatiles sans user de la mémoire interne du micro-contrôleur plus que nécessaire.

## Comment :

```arduino
#include <SD.h>

File tempFile;

void setup() {
  Serial.begin(9600);
  while (!Serial) {
    ; // attendre la connexion du port série
  }

  if (!SD.begin(4)) {
    Serial.println("Erreur d'initialisation de la carte SD");
    return;
  }
  
  tempFile = SD.open("temp.txt", FILE_WRITE); // ouverture en écriture
  if (tempFile) {
    Serial.println("Ecriture dans le fichier temporaire...");
    tempFile.println("Ce texte est temporaire.");
    tempFile.close(); // fermer le fichier
  } else {
    Serial.println("Erreur en ouvrant le fichier temporaire.");
  }
}

void loop() {
  // Ton code ici...
}
```
Sortie :
```
Ecriture dans le fichier temporaire...
```

## Exploration :

Historiquement, la création de fichiers temporaires sur des ordinateurs a servi à conserver des données pendant des processus qui ne pouvaient pas être accomplis en une seule passe. Sur Arduino, avec son espace limité, on contourne cette limitation en écrivant sur une carte SD externe.

Alternatives : utiliser la mémoire EEPROM interne s'il ne s'agit que de petites quantités de données, ou bien la RAM si les données sont éphémères et de petite taille. N'oubliez pas, cependant, que l’EEPROM a un nombre limité d'écritures avant de s'user.

Détails d'implémentation : Prévoyez de gérer les échecs d'ouverture de fichier et assurez-vous de fermer le fichier pour économiser la mémoire et éviter la corruption des données.

## Voir Aussi :

- Documentation Arduino sur la gestion des fichiers : https://www.arduino.cc/en/Reference/SD
- Instructions pour utiliser la mémoire EEPROM sur Arduino : https://www.arduino.cc/en/Reference/EEPROM
- Détails sur la gestion de la mémoire sur Arduino : https://www.arduino.cc/en/Tutorial/Foundations/Memory
