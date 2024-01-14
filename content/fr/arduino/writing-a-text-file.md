---
title:                "Arduino: Écrire un fichier texte"
simple_title:         "Écrire un fichier texte"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/arduino/writing-a-text-file.md"
---

{{< edit_this_page >}}

# Pourquoi

L'écriture d'un fichier texte peut sembler être une tâche banale, mais cela joue un rôle important dans la programmation Arduino. En écrivant un fichier texte, vous pouvez stocker des informations importantes pour votre projet, telles que des données de capteur ou des paramètres de configuration.

## Comment faire

Pour écrire un fichier texte en utilisant Arduino, il existe plusieurs étapes à suivre:

- Tout d'abord, vous devez inclure la bibliothèque "SD" dans votre code.
- Ensuite, vous devez initialiser une carte SD en utilisant la fonction "SD.begin()".
- Créez un objet "File" en utilisant la fonction "open()", qui spécifie le nom du fichier et le mode d'accès (lecture, écriture, etc.).
- Utilisez la fonction "println()" pour écrire des données dans le fichier.
- Enfin, n'oubliez pas de fermer le fichier en utilisant la fonction "close()".

Voici un exemple de code pour écrire un fichier texte appelé "donnees.txt" et y stocker une valeur de température:

```Arduino
#include <SD.h>

void setup() {
  // initialiser la carte SD
  SD.begin(10);

  // ouvrir le fichier en mode écriture
  File myFile = SD.open("donnees.txt", FILE_WRITE);

  // vérifier si le fichier s'est correctement ouvert
  if (myFile) {
    // écrire la valeur de température dans le fichier
    myFile.println("Température: 25°C");
    // fermer le fichier
    myFile.close();
  } else {
    // si le fichier ne s'est pas ouvert, imprimer un message d'erreur
    Serial.println("Erreur lors de l'ouverture du fichier.");
  }
}

void loop() {
  // ne rien faire dans la boucle principale
}
```

Ensuite, si vous ouvrez le fichier "donnees.txt", vous devriez voir la valeur de température stockée à l'intérieur.

## Plongée en profondeur

Il est important de noter que vous pouvez également écrire plusieurs lignes de données dans un fichier en utilisant une boucle et la fonction "println()". De plus, vous pouvez également spécifier un chemin d'accès pour votre fichier si vous souhaitez enregistrer dans un dossier spécifique sur votre carte SD.

Vous pouvez également utiliser la fonction "print()" pour écrire des données sans ajouter automatiquement une nouvelle ligne à chaque fois. Cela peut être utile si vous souhaitez écrire des données dans un format spécifique, comme une liste.

## Voir aussi

Voici quelques liens utiles pour en savoir plus sur l'écriture de fichiers texte avec Arduino:

- Tutoriel officiel de la bibliothèque SD: https://www.arduino.cc/en/Reference/SD
- Tutoriel sur l'écriture de fichiers sur une carte SD avec Arduino: https://randomnerdtutorials.com/guide-for-microsd-card-module-with-arduino/
- Exemples de code pour écrire et lire des fichiers texte avec Arduino: https://create.arduino.cc/projecthub/projects/tags/text+file