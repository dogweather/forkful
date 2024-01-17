---
title:                "Travailler avec les fichiers csv"
html_title:           "Arduino: Travailler avec les fichiers csv"
simple_title:         "Travailler avec les fichiers csv"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/arduino/working-with-csv.md"
---

{{< edit_this_page >}}

## Qu’est-ce que c'est et pourquoi le faire?

CSV (Comma-Separated Values) est un format de fichier souvent utilisé pour stocker des données tabulaires telles que des feuilles de calcul. Les programmeurs utilisent du CSV pour faciliter l'échange de données entre différentes applications et systèmes. Cela permet également de séparer facilement les données en utilisant des virgules, ce qui les rend pratiques pour la manipulation de grandes quantités de données.

## Comment faire:

Voici un exemple simple de comment créer et écrire des données dans un fichier CSV en utilisant Arduino:

```Arduino
#include <SD.h> //inclure la bibliothèque pour la carte SD

void setup() {
  Serial.begin(9600); //initialiser la communication série
  
  //vérifier si la carte SD est présente
  if (!SD.begin(10)) {
    Serial.println("Impossible de trouver la carte SD.");
    while (1);
  }
  
  //ouvrir le fichier CSV en mode écriture
  File dataFile = SD.open("donnees.csv", FILE_WRITE); 
  
  //écrire des données dans le fichier sous le format: nom, prénom, âge
  dataFile.println("Dupont, Jean, 25");
  dataFile.println("Martin, Marie, 30");
  dataFile.println("Durand, Pierre, 40");
  
  //fermer le fichier
  dataFile.close(); 
}

void loop() {
  //ne rien faire dans la boucle loop()
}
```

Lorsque ce code est exécuté, un nouveau fichier "donnees.csv" sera créé sur votre carte SD contenant les données que vous avez écrites. Voici un exemple du contenu de ce fichier:

```
Dupont, Jean, 25
Martin, Marie, 30
Durand, Pierre, 40
```

## Plongeon en profondeur:

### Contexte historique:

Le format CSV a été inventé dans les années 1960 pour faciliter l'échange de données entre des feuilles de calcul. Aujourd'hui, il est largement utilisé dans de nombreux domaines, tels que le développement web et les bases de données.

### Alternatives:

Il existe plusieurs autres formats de fichiers pour stocker des données structurées, tels que JSON et XML. Cependant, CSV reste populaire pour son simplicité et sa facilité d'utilisation.

### Détails d'implémentation:

Arduino a une bibliothèque SD intégrée qui permet de lire et d'écrire des données dans des fichiers CSV sur une carte SD. Cependant, cette bibliothèque est limitée en termes de fonctionnalités. Si vous avez besoin de fonctionnalités plus avancées, il est recommandé d'utiliser une bibliothèque tierce spécialement conçue pour travailler avec CSV.

## Voir aussi:

- La documentation officielle d'Arduino sur la programmation de cartes SD: https://www.arduino.cc/en/Reference/SD
- Une bibliothèque tierce pour travailler avec CSV sur Arduino: https://github.com/ifreecarve/SD_Card_Manager