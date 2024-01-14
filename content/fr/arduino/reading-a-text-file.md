---
title:    "Arduino: Lecture d'un fichier texte"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

## Pourquoi

L'utilisation de fichiers texte dans la programmation Arduino peut être extrêmement utile pour stocker et accéder à des données telles que des valeurs ou des instructions. Ceci peut être particulièrement utile si vous avez besoin de stocker une grande quantité d'informations ou de données que vous souhaitez accéder rapidement.

## Comment faire

Pour lire un fichier texte dans un code Arduino, suivez les étapes ci-dessous :

1. Tout d'abord, vous devrez inclure la bibliothèque SD dans votre programme en utilisant :


```Arduino
#include <SD.h>
```

2. Ensuite, vous devrez initialiser le module de carte SD en utilisant la fonction ``SD.begin()`` et en spécifiant le numéro de broche pour la communication :

```Arduino
if(!SD.begin(10)){
  Serial.println("Erreur lors de l'initialisation de la carte SD");
  return;
}
```

3. Ensuite, vous devrez ouvrir le fichier texte en utilisant la fonction ``SD.open()`` et en passant le nom du fichier ainsi que le mode d'accès comme paramètres :

```Arduino
File myFile = SD.open("fichier.txt", FILE_READ);
if(!myFile){
  Serial.println("Erreur lors de l'ouverture du fichier");
  return;
}
```

4. Enfin, vous pouvez lire le contenu du fichier en utilisant la fonction ``myFile.read()`` et en stockant les données dans une variable :

```Arduino
int data = myFile.read();
```

## Plongée profonde

Il est important de noter que la lecture de fichiers texte sur un microcontrôleur comme l'Arduino peut être assez lente, surtout si vous avez un grand nombre de données à lire. Il est donc préférable d'utiliser des méthodes de compression de données pour réduire la taille du fichier et améliorer les performances de lecture.

De plus, il est également important de prendre en compte le format du fichier texte, car il peut varier en fonction du système d'exploitation utilisé. Il est donc recommandé d'utiliser des outils de traitement de texte tels que Notepad++ pour s'assurer que le fichier est au bon format avant de le lire sur l'Arduino.

Enfin, il est important de gérer les erreurs lors de la lecture du fichier, en utilisant des instructions ``if`` pour vérifier si le fichier s'est ouvert correctement et pour gérer les éventuelles erreurs de lecture.

## Voir aussi

- [Guide complet pour utiliser une carte SD avec Arduino](https://www.arduino.cc/en/Reference/SD)
- [Tutoriel vidéo pour lire et écrire des fichiers texte sur une carte SD avec Arduino](https://www.youtube.com/watch?v=qKuMuNE7pEI)
- [Utilisation du format CSV pour stocker et lire des données sur l'Arduino](https://www.instructables.com/id/A-simple-data-logger-for-the-Arduino-and-or-SD-car/)