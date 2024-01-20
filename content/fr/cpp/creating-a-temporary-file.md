---
title:                "Création d'un fichier temporaire"
html_title:           "Kotlin: Création d'un fichier temporaire"
simple_title:         "Création d'un fichier temporaire"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/cpp/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

# Création de fichiers temporaires en C++

## Qu'est-ce que c'est & Pourquoi ?

La création d'un fichier temporaire consiste à générer un fichier pour y stocker des données uniquement pendant la durée d'exécution d'un programme. Ceci est très utile pour gérer de grandes quantités de données, sans surcharger la mémoire vive.

## Comment faire

Pour créer un fichier temporaire en C++, nous utilisons la bibliothèque `<cstdio>`. Voici comment:

```C++
#include <cstdio>

int main(){
    char cheminTemp[] = "/tmp/fichierXXXXXX";
    int descripteur = mkstemp(cheminTemp);

    if (descripteur == -1) {
        perror("Erreur lors de la création du fichier temporaire");
        return 1;
    }
    printf("Fichier temporaire créé : %s\n", cheminTemp);
    close(descripteur);
    return 0;
}
```

Lorsqu'exécuté, ce code générera une sortie similaire à ceci:

```
Fichier temporaire créé : /tmp/fichierf3Im0
```

## Plongée profonde

Créer un fichier temporaire n'est pas une fonction native du C++, mais une convention issue de l'ère Unix, où `/tmp` est un répertoire pour stocker les fichiers temporaires.

Il existe des alternatives pour créer des fichiers temporaires, comme l'utilisation de la bibliothèque Boost. Cependant, l'approche présentée ici est celle qui se conforme le plus au standard POSIX.

Lors de l'utilisation de `mkstemp()`, elle modifie le modèle passé, remplaçant les 'X' par des caractères uniques pour éviter les conflits de nom de fichier.

## À voir également

- La documentation de GNU pour `mkstemp()`: https://www.gnu.org/software/libc/manual/html_node/Temporary-Files.html
- Un guide plus détaillé pour gérer les fichiers temporaires avec C++ : https://cplusplus.com/articles/tmpfile/
- L'approche alternative avec la bibliothèque Boost : https://www.boost.org/doc/libs/1_75_0/libs/io/doc/temp_file_guide.html