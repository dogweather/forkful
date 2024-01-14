---
title:    "Arduino: Écrire un fichier texte"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/arduino/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Pourquoi

Écrire un fichier texte peut sembler une tâche simple et banale, mais c'est en réalité un aspect très important de la programmation Arduino. Les fichiers texte sont utilisés pour stocker des données, des paramètres ou encore des messages, et peuvent être lus et écrits par un microcontrôleur Arduino. Dans cet article, nous allons vous expliquer pourquoi il est essentiel de savoir écrire un fichier texte en programmation Arduino et comment le faire.

## Comment faire

Pour écrire un fichier texte en utilisant Arduino, vous devez suivre ces étapes simples :

1. Tout d'abord, vous devez ouvrir le fichier en mode écriture en utilisant la fonction `File.write()` et en spécifiant le nom du fichier ainsi que le mode d'écriture. Par exemple : 

```arduino
File myFile = SD.open("fichier.txt", FILE_WRITE);
```

2. Ensuite, vous pouvez écrire dans le fichier en utilisant la fonction `myFile.write()`, en spécifiant les données que vous souhaitez écrire. Par exemple : 

```arduino
myFile.write("Bonjour tout le monde !");
```

3. Une fois que vous avez fini d'écrire dans le fichier, vous devez le fermer en utilisant la fonction `myFile.close()`. Cela permet de s'assurer que toutes les données sont enregistrées de manière sécurisée.

4. Vous pouvez également ajouter des variables ou des valeurs dans votre fichier texte en utilisant la concaténation de chaînes de caractères. Par exemple : 

```arduino
int valeur = 5;
myFile.write("La valeur est : " + String(valeur));
```

## Plongée en profondeur

Il est important de noter que les fichiers texte sont stockés sur une carte SD ou dans la mémoire de l'Arduino, en fonction du type de carte utilisée. Pour utiliser une carte SD, vous devez inclure la bibliothèque SD en utilisant la fonction `#include <SD.h>` au début de votre programme.

De plus, n'oubliez pas de vérifier que votre microcontrôleur Arduino prend en charge l'écriture de fichiers texte. Certains modèles, comme l'Arduino Uno, ne disposent pas d'une carte SD intégrée, alors que d'autres, comme l'Arduino Mega, en ont une.

Il est également important de bien gérer la mémoire lors de l'écriture de fichiers texte. Assurez-vous de fermer les fichiers après utilisation pour éviter tout problème de mémoire et utilisez des variables de taille appropriée pour stocker les données.

## Voir aussi

- [Tutoriel complet sur l'écriture de fichiers texte en Arduino](https://www.arduino.cc/en/Reference/SD)
- [Guide de référence Arduino pour l'écriture de fichiers](https://www.arduino.cc/reference/en/language/functions/files-and-dirs/)

En utilisant ces informations, vous devriez maintenant être en mesure d'écrire des fichiers texte en toute confiance dans vos futurs projets Arduino. N'hésitez pas à expérimenter avec différentes fonctions et options pour découvrir toutes les possibilités que cela peut offrir. Bonne chance et amusez-vous bien !