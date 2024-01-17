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

## Quoi & Pourquoi ?

Lecture d'un fichier texte, c'est quand un programme prend des informations stockées dans un fichier texte et les utilise pour effectuer des actions spécifiques. Les programmeurs font cela pour obtenir des données structurées qu'ils peuvent traiter et utiliser dans leurs codes.

## Comment faire :

```Arduino
#include <SD.h> // inclure la bibliothèque SD pour lire les fichiers texte
File myFile; // créer une variable pour stocker le fichier texte

void setup() {
  // initialiser la communication avec la carte SD
  Serial.begin(9600);
  while (!Serial) {
    ; // attendre la connexion avec l'ordinateur pour continuer
  }

  if (!SD.begin(4)) {
    Serial.println("Echec de l'initialisation de la carte SD !");
    return;
  }
  Serial.println("Lecture du fichier texte :");

  // ouvrir le fichier texte et vérifier s'il s'ouvre correctement
  myFile = SD.open("monfichier.txt");
  if (myFile) {
    // lire le fichier jusqu'à la fin
    while (myFile.available()) {
      Serial.write(myFile.read());
    }
    myFile.close();
  }

  // si le fichier ne s'est pas ouvert correctement, afficher un message d'erreur
  else {
    Serial.println("Erreur lors de l'ouverture du fichier !");
  }
}

void loop() {
  // ne pas effectuer d'autres actions dans la boucle principale
}
```

## Plongée en profondeur :

La lecture de fichiers texte est une technique couramment utilisée par les programmeurs depuis le début de l'informatique. Elle est souvent préférée aux autres méthodes de stockage de données en raison de sa simplicité et de sa compatibilité avec de nombreux langages de programmation. Cette méthode peut également être utilisée pour lire des données à partir de cartes mémoire ou de périphériques de stockage externes.

## Voir aussi :

- [Tutorial sur l'utilisation de la carte SD avec Arduino](https://www.arduino.cc/en/Reference/SD)
- [Documentation officielle de la bibliothèque SD pour Arduino](https://www.arduino.cc/en/Reference/SDCard)
- [Exemples de codes pour lire un fichier texte avec Arduino](https://github.com/arduino-libraries/SD/blob/master/examples/ReadWrite/ReadWrite.ino)