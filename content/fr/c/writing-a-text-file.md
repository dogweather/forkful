---
title:                "C: Écriture d'un fichier texte"
simple_title:         "Écriture d'un fichier texte"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous êtes un programmeur en herbe ou même un vétéran de la programmation, vous avez peut-être entendu parler de la création de fichiers texte en C. Mais pourquoi quelqu'un voudrait-il écrire un fichier texte en premier lieu?

Eh bien, la réponse est simple - les fichiers texte sont l'un des moyens les plus simples et les plus polyvalents de stocker et de manipuler des données dans un programme. Ils sont également compatibles avec la plupart des langages de programmation et faciles à manipuler pour les utilisateurs finaux.

## Comment faire

Écrire un fichier texte en C est assez simple et peut être fait avec quelques lignes de code. Consultez l'exemple ci-dessous:

```C 
#include <stdio.h> //Inclure la bibliothèque standard d'E/S

int main() {

    FILE *fichier; //Déclarer un pointeur de fichier
    fichier = fopen("monfichier.txt", "w"); //Ouvrir le fichier en mode écriture

    //Écrire du texte dans le fichier
    fprintf(fichier, "Ceci est un fichier texte créé en utilisant C");

    //Fermer le fichier
    fclose(fichier);

    return 0;
}
```

Lorsque vous exécutez le code ci-dessus, il créera un fichier texte nommé "monfichier.txt" et y écrira la phrase "Ceci est un fichier texte créé en utilisant C". Vous pouvez également utiliser la fonction `fputc()` pour écrire un seul caractère et `fputs()` pour écrire une chaîne de caractères dans le fichier.

## Plongée en profondeur

Maintenant que vous savez comment écrire un fichier texte en utilisant C, il est important de comprendre comment cela fonctionne réellement. Lorsque vous ouvrez un fichier en mode écriture, l'ordinateur alloue un bloc d'espace pour que vous puissiez écrire vos données. Toutes les données écrites dans le fichier seront stockées dans ce bloc jusqu'à ce que le fichier soit fermé. Si vous écrivez une quantité de données plus grande que l'espace alloué, les données supplémentaires seront perdues.

Il est également important de mentionner que vous pouvez également lire à partir d'un fichier texte en utilisant C en utilisant la fonction `fgetc()` pour lire un seul caractère et `fgets()` pour lire une ligne de texte. Vous pouvez également utiliser la fonction `fscanf()` pour formater la lecture des données à partir du fichier.

## Voir aussi

- [Guide pratique pour créer, lire et écrire un fichier texte en C] (https://www.geeksforgeeks.org/basics-file-handling-c/#:~:text=%20%201.,Noter%20que%20tout%20fichier%20est%20cr%C3%A9%C3%A9%20en%20tant%20que%20%20%E2%80%9Cfichier.%E2%80%9D%20%20More%20)

- [Tutoriel sur les fichiers en C] (https://www.tutorialspoint.com/cprogramming/c_file_io.htm)

- [Documentation C pour les opérations sur les fichiers] (https://www.cplusplus.com/reference/cstdio/)

Maintenant que vous savez comment écrire un fichier texte en C, vous pouvez commencer à l'utiliser dans vos projets et vous familiariser avec toutes les fonctions et méthodes disponibles pour manipuler les fichiers. Bonne programmation!