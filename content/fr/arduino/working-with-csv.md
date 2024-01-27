---
title:                "Manipulation des fichiers CSV"
date:                  2024-01-19
html_title:           "Bash: Manipulation des fichiers CSV"
simple_title:         "Manipulation des fichiers CSV"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/arduino/working-with-csv.md"
---

{{< edit_this_page >}}

## What & Why?
En programmation Arduino, travailler avec des fichiers CSV (Comma-Separated Values, ou valeurs séparées par des virgules) permet de stocker et de récupérer des données structurées facilement. Les programmeurs utilisent CSV pour la simplicité de sa manipulation et sa compatibilité avec des tableurs et des bases de données.

## How to:
Pour travailler avec CSV sur Arduino, imaginons que nous avons un capteur de température et nous voulons enregistrer les données dans un fichier CSV.

```Arduino
#include <SD.h>
File monFichier;

void setup() {
  Serial.begin(9600);
  if (!SD.begin(4)) {
    Serial.println("Erreur d'initialisation de la carte SD");
    return;
  }
  monFichier = SD.open("donnees.csv", FILE_WRITE);

  if (monFichier) {
    monFichier.println("Heure,Température");
  }
}

void loop() {
  int temperature = analogRead(A0); // Remplacer par votre méthode de lecture
  String dataString = String(millis()) + "," + String(temperature);

  if (monFichier) {
    monFichier.println(dataString);
    monFichier.flush();
  }

  delay(2000); // Collecte de données toutes les 2 secondes
}
```

Sortie attendue dans "donnees.csv":
```
Heure,Température
0,123
2000,130
4000,125
...

```

## Deep Dive
Historiquement, le format CSV est utilisé depuis les premiers ordinateurs car il est simple et ne nécessite pas de logiciel spécifique pour être créé ou lu. Alternativement, on pourrait utiliser d'autres formats comme XML ou JSON, mais ils sont plus complexes et requièrent plus d'espace de stockage. Dans le contexte d'Arduino, où la mémoire est limitée, CSV est souvent privilégié. Lors de l'implémentation, notez que la bibliothèque SD d'Arduino est utilisée pour l'écriture sur des cartes SD et peut travailler avec différents types de fichier.

## See Also
- Documentation Arduino pour la bibliothèque `SD`: https://www.arduino.cc/en/Reference/SD
- Tutoriel sur la lecture de capteurs: https://www.arduino.cc/en/Tutorial/BuiltInExamples/ReadAnalogVoltage
- Explication détaillée du format CSV: https://tools.ietf.org/html/rfc4180
