---
title:    "Arduino: Écrire un fichier texte"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

# Pourquoi écrire un fichier texte avec Arduino ?

Écrire un fichier texte avec Arduino peut sembler un peu intimidant pour les débutants en programmation, mais c'est en fait une compétence très utile à apprendre. Non seulement cela vous permettra de stocker des données importantes, mais cela peut également vous aider à résoudre des problèmes et à créer des projets plus complexes.

# Comment écrire un fichier texte avec Arduino

Pour écrire un fichier texte avec Arduino, vous pouvez utiliser la bibliothèque "SD" intégrée à l'IDE Arduino. Tout d'abord, vous devrez connecter une carte SD à votre Arduino et la formater en tant que système de fichiers FAT16 ou FAT32. Ensuite, vous pouvez utiliser le code suivant pour écrire un fichier texte :

```Arduino
#include <SD.h> // charger la bibliothèque SD

File monFichier; // créer un objet File

void setup() {
  SD.begin(10); // initialiser la carte SD sur le port 10
  monFichier = SD.open("monfichier.txt", FILE_WRITE); // ouvrir un fichier à écrire
  if (monFichier) { // si le fichier est ouvert avec succès
    monFichier.println("Bonjour, je suis un fichier texte écrit avec Arduino !"); // écrire du texte dans le fichier
    monFichier.close(); // fermer le fichier
  }
}

void loop() {
  // code here
}
```

En utilisant la commande "monFichier.println()" vous pouvez écrire du texte dans le fichier, et avec la commande "monFichier.close()" vous vous assurez que toutes les données sont enregistrées avant de fermer le fichier.

# Plongée en profondeur

Il est également possible d'écrire dans un fichier texte sans utiliser la carte SD, en utilisant la mémoire EEPROM de l'Arduino. Cependant, cela peut être plus compliqué et nécessite une manipulation plus avancée des données. De plus, la mémoire EEPROM a une durée de vie limitée en termes de nombre d'écritures possibles, donc il est préférable d'utiliser une carte SD pour stocker des données en grande quantité.

# Voir aussi

- [Tutoriel Arduino : écrire dans un fichier texte](https://www.arduino.cc/en/Tutorial/LibraryExamples/WriteToSD)
- [Documentation de la bibliothèque SD](https://www.arduino.cc/en/Reference/SD)
- [Vidéo explicative : écrire un fichier texte avec Arduino](https://www.youtube.com/watch?v=GBNtK7LZjZs)