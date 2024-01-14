---
title:                "C: Écrire un fichier texte"
programming_language: "C"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Pourquoi

Ecrire un programme en langage C est une compétence essentielle dans le monde de la programmation. Un des aspects les plus utiles de la programmation en C est la capacité de créer et de manipuler des fichiers textes. Dans cet article, nous allons explorer pourquoi il est important de savoir écrire un fichier texte en langage C.

## Comment faire

Le processus d'écriture d'un fichier texte en C peut sembler intimidant au début, mais il est en réalité assez simple. Tout d'abord, vous devez inclure la bibliothèque "stdio.h" dans votre programme pour pouvoir utiliser les fonctions d'entrée/sortie standard. Ensuite, vous devez ouvrir le fichier en utilisant la fonction fopen() en spécifiant le nom du fichier et le mode d'ouverture ("w" pour écrire). En utilisant la fonction fprintf(), vous pouvez écrire du texte dans le fichier. Enfin, n'oubliez pas de fermer le fichier en utilisant la fonction fclose() lorsque vous avez terminé.

Voici un exemple de code pour écrire un fichier texte en C :

```C
#include <stdio.h>

int main(void) {
    // Ouvrir le fichier en mode écriture
    FILE *fichier = fopen("mon_fichier.txt", "w");

    // Ecrire du texte dans le fichier
    fprintf(fichier, "Bonjour le monde !");

    // Fermer le fichier
    fclose(fichier);

    return 0;
}
```

En exécutant ce code, un fichier texte appelé "mon_fichier.txt" sera créé dans le même répertoire que votre programme, contenant le texte "Bonjour le monde !".

## Plongée en profondeur

Maintenant que nous avons vu comment écrire un fichier texte en C, voyons pourquoi cette compétence est importante. Tout d'abord, en écrivant un fichier texte, vous pouvez stocker des informations de manière permanente. Ceci est utile pour les programmes qui doivent stocker des données, telles que des scores de jeu ou des bases de données. De plus, cela permet de créer des fichiers pour partager des données avec d'autres programmes ou utilisateurs.

Il est également important de savoir comment écrire un fichier texte en C pour pouvoir lire des fichiers. En utilisant les fonctions d'entrée/sortie standard, vous pouvez facilement lire le contenu d'un fichier texte, ce qui permet à votre programme d'utiliser ces données pour effectuer des tâches spécifiques.

Il est à noter que la manipulation de fichiers texte en C peut être plus complexe si vous avez besoin de lire ou d'écrire des données structurées, telles que des nombres ou des chaînes de caractères. Dans ce cas, vous devrez utiliser des fonctions telles que fscanf() pour lire les données d'un fichier et fprintf() pour les écrire.

## Voir aussi

- [Introduction à la programmation en C](https://fr.wikipedia.org/wiki/Programmation_en_C)
- [Fonctions d'entrée/sortie en C](https://www.programiz.com/c-programming/c-input-output)
- [Tutoriel sur la création et la lecture de fichiers en C](https://www.tutorialspoint.com/cprogramming/c_file_io.htm)