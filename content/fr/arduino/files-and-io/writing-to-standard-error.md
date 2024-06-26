---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:32:28.280240-07:00
description: "Comment faire : Arduino ne diff\xE9rencie pas nativement entre la sortie\
  \ standard et l'erreur standard comme le font les syst\xE8mes informatiques\u2026"
lastmod: '2024-03-13T22:44:58.131599-06:00'
model: gpt-4-0125-preview
summary: "Arduino ne diff\xE9rencie pas nativement entre la sortie standard et l'erreur\
  \ standard comme le font les syst\xE8mes informatiques conventionnels."
title: "\xC9crire sur l'erreur standard"
weight: 25
---

## Comment faire :
Arduino ne différencie pas nativement entre la sortie standard et l'erreur standard comme le font les systèmes informatiques conventionnels. Les méthodes `Serial.print()` et `Serial.println()` écrivent toutes deux sur la même sortie série, généralement visualisée dans le moniteur série de l'IDE Arduino. Cependant, nous pouvons émuler l'écriture sur stderr en formatant spécifiquement les messages d'erreur ou en les dirigeant vers une sortie alternative, telle qu'un fichier sur une carte SD ou via une connexion réseau.

Pour émuler stderr, vous pouvez préfixer les messages d'erreur avec une balise comme "ERREUR :" pour les différencier dans le moniteur série :

```cpp
void setup() {
  Serial.begin(9600); // Initialiser la communication série à un taux de 9600 bauds
}

void loop() {
  int result = someFunction();
  if (result == -1) {
    // Émuler stderr en préfixant le message d'erreur
    Serial.println("ERREUR : La fonction a échoué à s'exécuter.");
  } else {
    Serial.println("La fonction s'est exécutée avec succès.");
  }
  delay(1000); // Attendre une seconde avant de redémarrer la boucle
}

int someFunction() {
  // Une fonction fictive qui retourne -1 en cas d'erreur
  return -1;
}
```

Un exemple de sortie dans le moniteur série de l'IDE Arduino pourrait ressembler à ceci :

```
ERREUR : La fonction a échoué à s'exécuter.
```

Pour les projets nécessitant une approche plus sophistiquée, y compris l'écriture sur différentes sorties physiques, l'utilisation de bibliothèques tierces ou de matériel supplémentaire peut être nécessaire. Par exemple, logger les messages d'erreur sur une carte SD nécessite la bibliothèque `SD` :

```cpp
#include <SPI.h>
#include <SD.h>

File myFile;

void setup() {
  Serial.begin(9600);
  if (!SD.begin()) {
    Serial.println("ERREUR : L'initialisation de la carte SD a échoué !");
    return;
  }
  
  myFile = SD.open("error.log", FILE_WRITE);
  if (myFile) {
    myFile.println("ERREUR : La fonction a échoué à s'exécuter.");
    myFile.close(); // Assurez-vous de fermer le fichier pour sauvegarder le contenu
  } else {
    Serial.println("ERREUR : L'ouverture de error.log a échoué !");
  }
}

void loop() {
  // Votre code principal irait ici
}
```

Avec cette approche, vous séparez physiquement la sortie normale du programme des messages d'erreur en dirigeant ces derniers vers un fichier `error.log` sur une carte SD, permettant des analyses post-mortem sans encombrer le canal de sortie principal.
