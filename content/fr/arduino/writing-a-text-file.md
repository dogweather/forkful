---
title:                "Arduino: Ecriture d'un fichier texte"
programming_language: "Arduino"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/arduino/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous travaillez sur un projet Arduino et que vous avez besoin de stocker des données, la création d'un fichier texte peut être une solution simple et facile. Cela vous permet de stocker des informations telles que des paramètres, des valeurs de capteurs, ou même du texte que vous voulez afficher sur un écran LCD. Dans cet article, nous allons vous montrer comment écrire un fichier texte sur un Arduino.

## Comment faire

Tout d'abord, vous devez inclure la bibliothèque SD dans votre code. Cela vous permettra d'accéder à la carte SD connectée à votre Arduino. Ensuite, vous devez initialiser un objet SD en utilisant la fonction "begin" avec le numéro de broche de votre carte SD.

```Arduino
#include <SD.h>

//Initialiser un objet SD 
SD.begin(10);
```

Ensuite, vous pouvez créer un fichier texte avec la fonction "open". Vous devez spécifier le nom du fichier à créer, le type d'accès (lecture, écriture, ou les deux), et un Second paramètre facultatif pour spécifier le formatage du fichier (par exemple, "FILE_WRITE" pour ajouter du texte à la fin du fichier).

```Arduino
File monFichier = SD.open("data.txt", FILE_WRITE);
```

Maintenant que votre fichier est prêt à être utilisé, vous pouvez écrire du texte à l'aide de la fonction "print" ou "println". Vous pouvez également utiliser une variable à la place du texte pour écrire une valeur de capteur, par exemple.

```Arduino
monFichier.println("Bonjour le monde !");
monFichier.print("Ma valeur de capteur : ");
monFichier.println(valeurCapteur);
```

Enfin, n'oubliez pas de fermer le fichier une fois que vous avez terminé en utilisant la fonction "close". Cela assurera que toutes les données sont écrites sur le fichier et que vous pouvez y accéder ultérieurement.

```Arduino
monFichier.close();
```

Bravo ! Vous avez maintenant écrit un fichier texte sur votre Arduino. Vous pouvez accéder au fichier en le retirant de votre Arduino et en le connectant à un ordinateur via un lecteur de carte SD.

## Plongée en profondeur

Si vous souhaitez en savoir plus sur les fichiers textes et leur fonctionnement sur un Arduino, voici quelques informations supplémentaires :

- La taille maximale d'un fichier sur un Arduino est limitée à 2 Go.
- Vous pouvez également utiliser la fonction "exists" pour vérifier si un fichier existe déjà avant de le créer.
- Vous pouvez utiliser la fonction "flush" pour forcer l'écriture du contenu du fichier sur la carte SD immédiatement, au lieu d'attendre la fermeture du fichier.
- Pour lire un fichier texte, vous pouvez utiliser la fonction "readLine" pour lire une ligne spécifique ou simplement utiliser la fonction "read" pour lire l'ensemble du fichier.

## Voir aussi

- Tutoriel officiel d'Arduino sur l'utilisation de cartes SD : https://www.arduino.cc/en/Tutorial/ReadWrite
- Documentation officielle de la bibliothèque SD : https://www.arduino.cc/en/Reference/SD
- Tutoriel vidéo de Programming Electronics sur la création et l'écriture de fichiers texte sur un Arduino : https://www.youtube.com/watch?v=mmnkO6h7QJw