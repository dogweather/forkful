---
title:                "Lecture d'un fichier texte"
html_title:           "Arduino: Lecture d'un fichier texte"
simple_title:         "Lecture d'un fichier texte"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?
Lire un fichier texte en programmation est simplement l'acte d'extraire des informations stockées dans un fichier texte. C'est souvent nécessaire pour les développeurs, car cela permet de travailler avec des données dynamiques et persistantes, et pas seulement des données codées en dur dans le programme.

## Comment faire :
Voici une simple fonction en C pour lire un fichier texte :
```C
#include <stdio.h>

void lireFichier(const char* nomFichier) {
    FILE* fichier = fopen(nomFichier, "r");
    if (fichier == NULL) {
        printf("Erreur lors de l'ouverture du fichier %s", nomFichier);
        return;
    }

    char ch;
    while ((ch = fgetc(fichier)) != EOF) {
        putchar(ch);
    }

    fclose(fichier);
}
```
L'échantillon de sortie pourrait être le contenu du fichier texte.

## Plongée en profondeur
Historiquement, la lecture de fichiers est une fonctionnalité fondamentale de la plupart des systèmes d'exploitation depuis les débuts de l'informatique. En C, nous lisons des fichiers texte avec l'API stdio, mais il existe d'autres alternatives, par exemple, les fonctions de bas niveau définies dans `fcntl.h` ou les API de lecture de fichiers entièrement différentes disponibles dans certains systèmes d'exploitation comme `CreateFile` de Windows.

En termes d'implémentation, `fopen` ouvre un fichier et renvoie un `FILE*` pointeur. `fgetc` obtient un caractère à partir du fichier. Cela se fait généralement dans une boucle jusqu'à ce que le `EOF` (fin de fichier) soit détecté. Enfin, `fclose` ferme le fichier. 

## Voir aussi 
Pour en savoir plus sur les opérations de fichiers en C, visitez ces ressources :

- Tutoriel sur le site C Programming : https://www.cprogramming.com/tutorial/cfileio.html
- Documentation officielle GNU sur les E/S standard : https://www.gnu.org/software/libc/manual/html_node/Standard-Streams.html
- Article de Microsoft sur l'ouverture, la lecture et l'écriture de fichiers : https://docs.microsoft.com/fr-fr/cpp/c-runtime-library/opening-closing-reading-and-writing-to-files?view=msvc-160