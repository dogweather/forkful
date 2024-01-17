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

## Qu'est-ce que et pourquoi?

Créer un fichier temporaire est une pratique courante dans la programmation pour stocker des données ou des informations temporaires avant de les utiliser ou de les supprimer. Cela peut être utile lorsque vous avez besoin de stocker des données pour une courte période de temps et que vous ne voulez pas créer de fichiers permanents pour cela.

## Comment faire:

```Arduino
 // Créer un fichier temporaire nommé "temp.txt"
 File tempfile = SD.open("temp.txt", FILE_WRITE);
 // Écrire une chaîne de caractères dans le fichier
 tempfile.println("Bonjour le monde!");
 // Fermer le fichier
 tempfile.close();
```

## Plongée en profondeur:

La pratique de créer des fichiers temporaires existe depuis longtemps dans le domaine de la programmation. Elle est souvent utilisée pour stocker des données temporaires telles que des journaux, des mises en cache ou des fichiers temporaires pour les téléchargements. Les alternatives à la création de fichiers temporaires peuvent inclure l'utilisation de variables ou de tableaux pour stocker les données temporaires, mais cela peut être moins efficace et difficile à gérer pour les grandes quantités d'informations.

L'implémentation d'une fonction de création de fichiers temporaires peut varier en fonction du langage de programmation utilisé et du système d'exploitation sur lequel il s'exécute. Dans le cas d'Arduino, la création de fichiers temporaires peut être réalisée en utilisant les fonctions de la bibliothèque SD. Cela nécessite également un module de carte SD pour stocker les fichiers sur le système.

## Voir aussi:

- [Documentation Arduino sur la gestion des fichiers](https://www.arduino.cc/en/Reference/SD)
- [Article sur la création de fichiers temporaires en C++](https://www.geeksforgeeks.org/temporary-files-c/)
- [Différentes façons de gérer les données temporaires en programmation](https://www.digitalocean.com/community/tutorials/temporary-files-in-programming)