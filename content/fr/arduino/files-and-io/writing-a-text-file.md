---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:26:55.852652-07:00
description: "\xC9crire un fichier texte avec Arduino implique de sauvegarder des\
  \ donn\xE9es dans un fichier sur une carte SD ou un module de stockage similaire,\
  \ souvent dans\u2026"
lastmod: '2024-02-25T18:49:54.801274-07:00'
model: gpt-4-0125-preview
summary: "\xC9crire un fichier texte avec Arduino implique de sauvegarder des donn\xE9\
  es dans un fichier sur une carte SD ou un module de stockage similaire, souvent\
  \ dans\u2026"
title: "R\xE9diger un fichier texte"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?
Écrire un fichier texte avec Arduino implique de sauvegarder des données dans un fichier sur une carte SD ou un module de stockage similaire, souvent dans le but de consigner des données. Les programmeurs font cela pour enregistrer les lectures de capteurs, sauvegarder des configurations ou journaliser des événements d'application au fil du temps, cela étant crucial pour les projets nécessitant une analyse de données ou un suivi.

## Comment :
Pour écrire dans un fichier texte sur une carte SD en utilisant Arduino, vous devez d'abord inclure la bibliothèque `SD.h`, qui fournit les fonctions nécessaires pour interagir avec les cartes SD. Assurez-vous que votre carte Arduino est connectée à un module de carte SD.

```cpp
#include <SPI.h>
#include <SD.h>

File myFile;

void setup() {
  // Initialise la communication série à 9600 bits par seconde :
  Serial.begin(9600);
  
  // Vérifie l'initialisation de la carte SD
  if (!SD.begin(4)) {
    Serial.println("L'initialisation a échoué !");
    return;
  }
  Serial.println("Initialisation réussie.");
  
  // Ouvre le fichier. Notez qu'un seul fichier peut être ouvert à la fois,
  // donc vous devez fermer celui-ci avant d'en ouvrir un autre.
  myFile = SD.open("test.txt", FILE_WRITE);
  
  // Si le fichier s'ouvre correctement, écrivez dedans :
  if (myFile) {
    Serial.print("Écriture dans test.txt...");
    myFile.println("Test d'écriture de fichier texte.");
    // Ferme le fichier :
    myFile.close();
    Serial.println("terminé.");
  } else {
    // Si le fichier ne s'ouvre pas, affiche une erreur :
    Serial.println("Erreur à l'ouverture de test.txt");
  }
}

void loop() {
  // Rien ne se passe après le setup
}
```

### Sortie Exemple :
Lorsque vous exécutez ce code, le moniteur série de l'IDE Arduino affichera :
```
Initialisation réussie.
Écriture dans test.txt...terminé.
```
Pour vérifier si les données ont été correctement écrites, vous pouvez retirer la carte SD de l'Arduino, l'insérer dans un ordinateur et ouvrir le fichier `test.txt` pour voir le message "Test d'écriture de fichier texte."

Pour les projets nécessitant des opérations de fichier plus avancées ou un traitement, pensez à explorer des bibliothèques supplémentaires ou à écrire des fonctions personnalisées adaptées à vos besoins spécifiques.
