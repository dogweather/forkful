---
title:                "Lecture d'un fichier texte"
date:                  2024-01-20T17:53:51.036195-07:00
model:                 gpt-4-1106-preview
simple_title:         "Lecture d'un fichier texte"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c/reading-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why? (Quoi et Pourquoi ?)

Lire un fichier texte en C, c'est récupérer des données depuis un fichier sur votre disque pour les utiliser dans votre programme. Les programmeurs font ça pour traiter des informations en masse, configurer des programmes, ou simplement charger des textes.

## How to: (Comment faire :)

Voici un exemple simple pour lire un fichier ligne par ligne :

```c
#include <stdio.h>
#include <stdlib.h>

int main() {
    FILE *fichier;
    char ligne[100];

    fichier = fopen("exemple.txt", "r");
    if (fichier == NULL) {
        perror("Erreur à l'ouverture du fichier");
        return 1;
    }

    while (fgets(ligne, sizeof(ligne), fichier) != NULL) {
        printf("%s", ligne);
    }

    fclose(fichier);
    return 0;
}
```

Sortie d'exemple pour un fichier `exemple.txt` contenant "Bonjour le monde!":

```
Bonjour le monde!
```

## Deep Dive (Plongée Profonde)

Historiquement, C était la lingua franca de la programmation. Lire des fichiers est une partie essentielle depuis le début. Au fil du temps, les approches ont évolué, mais les fonctions standard de C restent de mise. En dehors de `fopen`, `fgets`, et `fclose`, vous pourriez rencontrer `fread` et `fwrite` pour des données binaires, ou des opérations de niveau système comme `open`, `read`, `write`, et `close` sur des systèmes UNIX-like.

Les alternatives actuelles comprennent les librairies comme POSIX en C, ou les fonctions de manipulation de fichiers en C++. Cependant, en C, les fonctions de la bibliothèque standard sont suffisantes pour la plupart des besoins et bien portées sur différentes plateformes.

Les détails d'implémentation à garder à l'esprit :
- Toujours vérifier si l'ouverture du fichier réussit pour éviter les erreurs.
- Utiliser `feof` pour vérifier la fin d'un fichier peut être trompeur – préférez la vérification du retour de `fgets` ou `fread`.
- Penser à gérer les chemins de fichiers de manière portable si votre programme doit être exécuté sur divers systèmes d'exploitation.

## See Also (Voir Aussi)

- La documentation de la bibliothèque C Standard pour plus de détails sur les fonctions de fichiers: https://en.cppreference.com/w/c/io
- Un guide sur les opérations de fichiers POSIX: https://en.wikipedia.org/wiki/POSIX
- Tutoriel C sur la gestion des fichiers: https://www.tutorialspoint.com/cprogramming/c_file_io.htm
