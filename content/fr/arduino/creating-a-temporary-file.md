---
title:                "Création d'un fichier temporaire"
html_title:           "Arduino: Création d'un fichier temporaire"
simple_title:         "Création d'un fichier temporaire"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/arduino/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Pourquoi

Vous vous demandez peut-être pourquoi créer un fichier temporaire en utilisant Arduino. Eh bien, il peut y avoir plusieurs raisons pour cela. Par exemple, vous pouvez utiliser un fichier temporaire pour stocker des données temporaires pendant l'exécution d'un programme ou pour tester votre code avant de l'implémenter de manière définitive.

## Comment faire

Voici un exemple simple de création et d'utilisation d'un fichier temporaire en utilisant Arduino :

```Arduino
#include <SD.h> //inclure la bibliothèque SD pour la gestion des fichiers

File myFile; //déclarer un objet de fichier temporaire

void setup() {
  Serial.begin(9600); //initialiser le port série pour afficher des messages
  
  SD.begin(4); //initialiser la carte SD avec l'indentificateur de broche 4
  
  //créer un fichier temporaire nommé "temp.txt" et ouvrir en mode écriture
  myFile = SD.open("temp.txt", FILE_WRITE);

  //écrire une chaîne de caractères dans le fichier
  if (myFile) {
    myFile.println("Ceci est un fichier temporaire !"); 
    myFile.close(); //fermer le fichier
    Serial.println("Fichier temporaire créé avec succès !");
  }
}

void loop() {
  //rien à faire dans la boucle principale
}
```

En exécutant ce code, vous verrez sur le moniteur série le message "Fichier temporaire créé avec succès !". Maintenant, si vous ouvrez la carte SD sur votre ordinateur, vous devriez trouver le fichier "temp.txt" contenant la ligne de texte que nous avons écrite.

## Plongez plus en profondeur

Créer un fichier temporaire peut également être utile lorsque vous avez besoin de stocker des données volumineuses qui peuvent ralentir votre programme si elles sont stockées en mémoire RAM. Dans ce cas, vous pouvez écrire les données dans un fichier temporaire, puis les récupérer en utilisant des fonctions de lecture de fichiers. Vous pouvez également choisir de supprimer le fichier temporaire une fois que vous avez terminé d'utiliser les données pour libérer de l'espace sur votre carte SD.

## Voir aussi

Voici quelques liens utiles pour en savoir plus sur la gestion des fichiers avec Arduino :

- [Documentation officielle de la bibliothèque SD](https://www.arduino.cc/en/Reference/SD)

- [Tutoriel sur la création et la lecture de fichiers avec Arduino](https://www.arduino.cc/en/Tutorial/ReadWrite)

- [Exemples de projets utilisant des fichiers temporels avec Arduino](https://create.arduino.cc/projecthub/projects/tags/file)

Maintenant que vous savez comment créer un fichier temporaire, vous pouvez l'appliquer à vos projets pour améliorer leur efficacité et leur gestion des données. Amusez-vous bien !