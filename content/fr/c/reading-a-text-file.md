---
title:    "C: Lecture d'un fichier texte"
keywords: ["C"]
---

{{< edit_this_page >}}

## Pourquoi

Il y a de nombreuses raisons pour lesquelles quelqu'un pourrait vouloir lire un fichier texte en langage C. Peut-être que vous devez extraire des données spécifiques à partir du fichier ou peut-être que vous voulez simplement vérifier son contenu. Quelle que soit la raison, la lecture de fichiers texte est une compétence utile pour tout programmeur C.

## Comment faire

Voici un exemple de code C pour lire un fichier texte :

```C
#include <stdio.h>

int main()
{
  // Ouvrir le fichier
  FILE *fichier = fopen("nom_du_fichier.txt", "r");
  
  // Vérifier si le fichier est ouvert correctement
  if (fichier == NULL)
  {
    printf("Erreur lors de l'ouverture du fichier.");
    return 1;
  }
  
  // Lire le fichier ligne par ligne
  char ligne[100];
  while (fgets(ligne, 100, fichier) != NULL)
  {
    printf("%s", ligne); // Afficher la ligne sur la console
  }
  
  // Fermer le fichier
  fclose(fichier);
  return 0;
}
```

Voici un exemple de contenu du fichier texte "nom_du_fichier.txt" :

```
Bonjour à tous !
Je suis un fichier texte.
J'ai plusieurs lignes de texte.
Amusez-vous à me lire !
```

Voici la sortie du programme :

```
Bonjour à tous !
Je suis un fichier texte.
J'ai plusieurs lignes de texte.
Amusez-vous à me lire !
```

Dans cet exemple, nous avons ouvert le fichier en mode "r" pour "lecture". Ensuite, nous avons utilisé la fonction "fgets" pour lire chaque ligne du fichier. Enfin, nous avons affiché chaque ligne sur la console. Il est important de noter que le tableau "ligne" doit être assez grand pour contenir la ligne la plus longue du fichier.

## Plongée Profonde

Il existe de nombreuses autres fonctions en langage C pour lire des fichiers texte. Par exemple, vous pouvez utiliser la fonction "fgetc" pour lire un caractère à la fois, ou la fonction "fscanf" pour lire des données formatées. Vous pouvez également utiliser des fonctions de positionnement de fichier pour naviguer à travers le fichier et lire uniquement les parties dont vous avez besoin.

Il est également important de mentionner que la lecture de fichiers texte peut être sujette à des erreurs si le fichier contient des caractères spéciaux ou si le format du fichier n'est pas correct. Il est donc important de traiter ces cas d'erreur pour garantir que votre programme fonctionne correctement.

## Voir aussi

- [Documentation sur la lecture et l'écriture de fichiers en C](https://www.tutorialspoint.com/c_standard_library/c_function_fopen.htm)
- [Autres exemples de code pour la lecture de fichiers texte en C](https://www.programiz.com/c-programming/c-file-input-output)
- [Article sur les fichiers texte en C](https://www.geeksforgeeks.org/basics-file-handling-c/)