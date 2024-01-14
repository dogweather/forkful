---
title:    "C: Écriture d'un fichier texte"
keywords: ["C"]
---

{{< edit_this_page >}}

## Pourquoi

Écrire un fichier texte peut sembler une tâche simple et banale en programmation, mais c'est en fait une étape importante dans de nombreuses applications. Les fichiers texte sont utilisés pour stocker de grandes quantités de données, pour les utiliser comme configuration ou pour écrire des fichiers journaux. Savoir comment écrire un fichier texte correctement est donc essentiel pour tout programmeur.

## Comment Faire

Pour écrire un fichier texte en utilisant le langage de programmation C, nous devons suivre quelques étapes simples. Tout d'abord, nous devons inclure la bibliothèque standard "stdio.h" en utilisant la directive #include. Ensuite, nous devons ouvrir un fichier en utilisant la fonction fopen(). Cette fonction prend deux arguments : le nom du fichier à ouvrir et le mode d'ouverture. Par exemple, si nous voulons ouvrir un fichier en mode écriture, nous utilisons le mode "w". Ensuite, nous pouvons utiliser la fonction fprintf() pour écrire du contenu dans le fichier, en spécifiant le format de ce que nous voulons écrire. Enfin, nous devons fermer le fichier à l'aide de la fonction fclose() pour éviter les erreurs de lecture ou d'écriture.

```
#include <stdio.h>

int main() {

    // Ouverture du fichier en mode écriture
    FILE *file = fopen("monfichier.txt", "w");

    // Écriture dans le fichier en utilisant le format '%s'
    fprintf(file, "%s", "Bonjour, je suis un fichier texte.");

    // Fermeture du fichier
    fclose(file);

    return 0;
}
```

Lorsque nous exécutons ce code, il va créer un fichier nommé "monfichier.txt" dans le même dossier que notre programme et y écrire le contenu "Bonjour, je suis un fichier texte.". Nous pouvons également utiliser la fonction putc() pour écrire un caractère à la fois ou fputs() pour écrire une chaîne de caractères.

## Plongeon Profond

Maintenant que nous avons vu comment écrire un fichier texte de base, il est important de comprendre quelques notions plus avancées. Premièrement, lorsque nous utilisons la fonction fprintf(), nous pouvons spécifier plusieurs formats pour écrire différentes variables dans le fichier. Par exemple, si nous voulons écrire une variable entière, nous utilisons le format "%d". Deuxièmement, il est important de manipuler correctement les erreurs de lecture ou d'écriture en utilisant les fonctions feof() et ferror(). Enfin, nous pouvons également utiliser des structures de données telles que les tableaux pour écrire de grandes quantités de données dans un fichier.

## Voir Aussi

- [Documentation de la fonction fopen()](https://www.tutorialspoint.com/c_standard_library/c_function_fopen.htm)
- [Tutoriel sur les fichiers en C](https://openclassrooms.com/fr/courses/19980-apprenez-a-programmer-en-c/19502-les-files-en-c) 
- [Guide sur les directives #include en C](https://www.geeksforgeeks.org/cc-preprocessors/#include)