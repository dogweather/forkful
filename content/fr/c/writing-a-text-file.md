---
title:    "C: Écriture d'un fichier texte"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/c/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Pourquoi écrire un fichier texte?

Ecrire un fichier texte est une des tâches les plus courantes en programmation. Cela permet d'enregistrer des données de manière permanente, ce qui est essentiel pour de nombreuses applications telles que les bases de données, les jeux, et les logiciels de traitement de texte.

## Comment faire?

Pour écrire un fichier texte en C, vous devrez utiliser les fonctions  `fopen()` pour ouvrir le fichier, `fprintf()` pour écrire dedans et `fclose()` pour le fermer. Voici un exemple de code:

```
#include <stdio.h>

int main() {
    FILE *fichier;
    fichier = fopen("mon_fichier.txt", "w"); //ouvrir le fichier en mode écriture
    
    if (fichier == NULL) { //vérifier si l'ouverture a réussi
        printf("Erreur lors de l'ouverture du fichier.");
        return 1;
    }
    
    fprintf(fichier, "Bonjour!"); //écrire dans le fichier
    fclose(fichier); //fermer le fichier
    
    return 0;
}
```

Cela va créer un fichier texte appelé "mon_fichier.txt" et y écrire le mot "Bonjour!". Vous pouvez également utiliser la fonction `fgets()` pour écrire plusieurs lignes dans le fichier.

## Plongée en profondeur

Lorsque vous écrivez un fichier texte, il est important de prendre en compte le format des données que vous y stockez. Les fichiers texte sont simplement une série de caractères, donc si vous avez besoin de stocker des données structurées, vous devrez les formater correctement avant de les écrire dans le fichier.

De plus, il est important de s'assurer que le chemin d'accès du fichier est correct et que vous avez les bonnes permissions pour écrire dedans. Si vous travaillez sur plusieurs systèmes d'exploitation, vous devrez peut-être utiliser des fonctions spécifiques telles que `fopen_s()` pour une compatibilité maximale.

## Voir aussi

- [Guide de référence C - Gestion des fichiers](https://www.tutorialspoint.com/c_standard_library/c_function_fopen.htm)
- [Cours sur les fichiers en C](https://www.cours-gratuit.com/cours-langage-c/les-fichiers-en-langage-c)
- [Tutoriel vidéo sur l'écriture de fichiers en C](https://www.youtube.com/watch?v=5Nf6GGSlfNs)