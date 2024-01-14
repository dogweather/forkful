---
title:    "C: Création d'un fichier temporaire"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/c/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Pourquoi
Les fichiers temporaires sont souvent utilisés dans la programmation pour stocker temporairement des données avant de les écrire sur un fichier permanent. Ils sont également utiles pour stocker des données temporaires lors de l'exécution d'un programme afin de les utiliser ultérieurement.

## Comment faire
Pour créer un fichier temporaire en langage C, vous pouvez utiliser la fonction `tmpfile()` qui crée un fichier vide et renvoie un pointeur vers ce fichier. Ensuite, vous pouvez écrire des données dans ce fichier à l'aide des fonctions `fprintf()` ou `fwrite()` et les lire à l'aide des fonctions `fscanf()` ou `fread()`. Voici un exemple de code:

```C
#include <stdio.h>

int main()
{
    FILE *fptr;
    int num = 10;
    float dec = 3.14;

    fptr = tmpfile(); // création du fichier temporaire
    fprintf(fptr, "%d\n", num); // écriture des données dans le fichier
    fprintf(fptr, "%f\n", dec);
    fseek(fptr, 0, SEEK_SET); // déplacement du curseur au début du fichier
    fscanf(fptr, "%d", &num); // lecture des données depuis le fichier
    fscanf(fptr, "%f", &dec);

    printf("Fichier temporaire a été créé et les données sont: %d et %.2f\n", num, dec);

    fclose(fptr); // fermeture du fichier
    return 0;
}
```

Ci-dessous est l'exemple de sortie pour ce code:

```
Fichier temporaire a été créé et les données sont: 10 et 3.14
```

## Plongée en profondeur
La fonction `tmpfile()` utilise un espace de stockage temporaire sur le disque pour créer le fichier. Cet espace est généralement situé dans le répertoire de fichiers temporaires qui peut être spécifié dans le fichier de configuration du système ou déterminé par la valeur de la variable d'environnement `TMPDIR`. Il est important de noter que le fichier temporaire créé à l'aide de `tmpfile()` sera automatiquement supprimé à la fin du programme ou en cas d'erreur.

Pour une plus grande flexibilité, vous pouvez également utiliser la fonction `tmpnam()` qui génère un nom de fichier aléatoire pour un fichier temporaire. Vous pouvez ensuite utiliser ce nom pour créer et manipuler le fichier en utilisant les fonctions standards de manipulation de fichiers en langage C.

## Voir aussi
- [La fonction `tmpfile()` en C](https://www.tutorialspoint.com/c_standard_library/c_function_tmpfile.htm)
- [Créer et gérer des fichiers en C](https://www.tutorialspoint.com/cprogramming/c_file_io.htm)
- [Utilisation de fichiers temporaires en programmation C](https://www.techwalla.com/articles/how-to-use-temporary-files-in-c-programming)