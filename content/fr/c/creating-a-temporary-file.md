---
title:                "Création d'un fichier temporaire"
html_title:           "Kotlin: Création d'un fichier temporaire"
simple_title:         "Création d'un fichier temporaire"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est et Pourquoi ?

La création d'un fichier temporaire est un moyen de stocker des données qui n'ont qu'une durée de vie limitée. Les programmeurs en ont besoin pour gérer et manipuler les données sans consommer trop de mémoire vive.

## Comment faire 

Voici un exemple de code pour créer un fichier temporaire en C :

```C
#include <stdio.h>

int main() {
    FILE * temp = tmpfile();
    if (temp == NULL) {
        printf("Impossible de créer le fichier temporaire.\n");
        return -1;
    }
    printf("Fichier temporaire créé avec succès.\n");
    fclose(temp);
    return 0;
} 
```

Quand vous lancez ce programme, il devrait afficher : "Fichier temporaire créé avec succès."

## En profondeur 

Historiquement, la création de fichiers temporaires a été introduite pour optimiser l'utilisation de la mémoire. Les fichiers temporaires sont supprimés automatiquement après la fermeture, ce qui est plus efficace que l’utilisation de la mémoire vive pour des données de grande taille.

En ce qui concerne les alternatives, on peut créer un fichier temporaire manuellement en utilisant `fopen()`, mais c'est plus complexe et nécessite une manipulation manuelle du fichier.

En termes de détails d'implémentation, `tmpfile()` crée un fichier temporaire binaire unique pour l'écriture et la lecture. Cependant, vous ne connaîtrez pas son nom, ce qui est une fonction de sécurité pour éviter l'accès non autorisé.

## Pour aller plus loin 

- Documentation du GCC : [https://gcc.gnu.org/onlinedocs/](https://gcc.gnu.org/onlinedocs/)
- Page de manuel de tmpfile [http://man7.org/linux/man-pages/man3/tmpfile.3.html](http://man7.org/linux/man-pages/man3/tmpfile.3.html)