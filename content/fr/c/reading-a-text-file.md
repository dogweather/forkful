---
title:                "Lecture d'un fichier texte"
html_title:           "C: Lecture d'un fichier texte"
simple_title:         "Lecture d'un fichier texte"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Pourquoi lire un fichier texte

Si vous êtes un programmeur C, il y a de fortes chances que vous ayez besoin de lire des fichiers texte à un moment donné. Que ce soit pour récupérer des données ou pour effectuer des opérations sur un fichier, il est important de savoir comment lire un fichier texte en utilisant ce langage de programmation. Dans cet article, nous vous expliquerons comment le faire de manière simple et concise.

## Comment faire

Pour lire un fichier texte en C, nous utiliserons la fonction `fopen()` pour ouvrir le fichier et la fonction `fscanf()` pour lire son contenu. Voici un exemple de code :

```
FILE *fichier;
char mot[20];

fichier = fopen("mon_fichier.txt", "r"); //ouverture en mode lecture

while(fscanf(fichier, "%s", mot) != EOF) { //boucle jusqu'à la fin du fichier
    printf("%s ", mot); //affichage du mot lu
}

fclose(fichier); //fermeture du fichier
```

En utilisant `fscanf()`, nous pouvons lire le contenu du fichier mot par mot. La boucle se répétera jusqu'à ce que nous atteignons la fin du fichier (EOF). À chaque itération, un mot sera stocké dans la variable `mot` et sera ensuite affiché à l'écran. Une fois tous les mots lus, nous fermons le fichier en utilisant `fclose()`.

## Plongée plus profonde

Il est également possible de lire un fichier texte ligne par ligne en utilisant la fonction `fgets()`. Cette fonction prend trois paramètres : un pointeur vers une chaîne de caractères qui stockera la ligne lue, la taille maximale de cette chaîne et un pointeur vers le fichier. Voici un exemple :

```
FILE *fichier;
char ligne[500];

fichier = fopen("mon_fichier.txt", "r");

while(fgets(ligne, 500, fichier) != NULL) { //boucle jusqu'à la fin du fichier
    printf("%s", ligne); //affichage de la ligne lue
}

fclose(fichier);
```

En utilisant `fgets()`, nous pouvons lire des fichiers avec des lignes de longueurs différentes et stocker chaque ligne dans la variable `ligne`.

## Voir aussi

- [Documentation complète sur `fopen()`](https://www.cplusplus.com/reference/cstdio/fopen/)
- [Documentation complète sur `fscanf()`](https://www.cplusplus.com/reference/cstdio/fscanf/)
- [Documentation complète sur `fgets()`](https://www.cplusplus.com/reference/cstdio/fgets/)