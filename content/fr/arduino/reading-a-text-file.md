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

## Qu'est-ce que c'est & Pourquoi?

Lire un fichier texte, c'est obtenir et utiliser les données stockées sous forme de texte. Les programmeurs l'utilisent pour accéder à des informations pré-existantes, pour analyser des données et pour gérer des ressources.

## Comment faire:

Pour lire un fichier texte sur Arduino, on utilise la bibliothèque SD. Voici un exemple de code:

```Arduino
#include <SD.h>

void setup(){
  Serial.begin(9600);
  while (!Serial) {;}
  if (!SD.begin(4)) {return;}
  File file = SD.open("test.txt");
  if (file) {
    while (file.available()) {
      Serial.write(file.read());
    }
    file.close();
  }
}

void loop() {}
```

Sortie prévisible:

```Arduino
Salut le monde!
```

## Plongée profonde:

La bibliothèque SD sur Arduino se base sur l'historique de ces deux technologies. Dans le passé, la lecture des fichiers textes se faisait principalement sur les gros systèmes informatiques. Avec l'évolution de la technologie et la miniaturisation des cartes micro SD, cette fonctionnalité est maintenant disponible sur la plate-forme Arduino.

Autrement, pour lire des données textuelles sur Arduino, on peut aussi utiliser la lecture série ou l'accès via Ethernet ou Wi-Fi.

L'implémentation dépend du type de carte Arduino que vous utilisez et de la manière dont vous avez configuré votre système de fichiers. Dans notre exemple précédent, nous avons utilisé la broche 4 pour la communication SD.

## Voir aussi:

Pour plus d'informations sur la programmation du système de fichiers Arduino, consultez les ressources suivantes:

- Bibliothèque SD Arduino: https://www.arduino.cc/en/reference/SD
- Lire les fichiers texte avec Python: https://learn.adafruit.com/adafruit-micro-sd-breakout-board-card-tutorial
- Accéder à des fichiers via Ethernet ou Wi-Fi: https://www.arduino.cc/en/Tutorial/LibraryExamples/ChatServer