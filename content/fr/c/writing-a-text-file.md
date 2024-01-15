---
title:                "Écrire un fichier texte"
html_title:           "C: Écrire un fichier texte"
simple_title:         "Écrire un fichier texte"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Pourquoi 
Que vous soyez un développeur débutant ou expérimenté, il est essentiel de savoir comment écrire un fichier texte en programmation C. Les fichiers texte sont une méthode courante pour stocker et gérer de grandes quantités de données à partir d'un programme. Cela peut être utile dans des situations telles que la sauvegarde de données utilisateur ou la lecture et l'écriture de fichiers de configuration.

## Comment faire
La première étape pour écrire un fichier texte en C est de déclarer un pointeur de fichier et de l'ouvrir en utilisant la fonction `fopen()`. Assurez-vous d'inclure la bibliothèque standard `stdio.h` pour utiliser cette fonction. Ensuite, utilisez la fonction `fprintf()` pour écrire les données dans le fichier texte en utilisant le format souhaité. Une fois que vous avez terminé d'écrire, n'oubliez pas de fermer le fichier en utilisant `fclose()`.

Voici un exemple de code pour écrire un fichier texte en C :

```C
#include <stdio.h>

int main()
{
   FILE *fptr; // déclaration du pointeur de fichier
   fptr = fopen("mon_fichier.txt", "w"); // ouvrir le fichier en mode écriture

   if (fptr == NULL)
   {
      printf("Erreur lors de l'ouverture du fichier.");
      return 1;
   }

   // écrire du texte dans le fichier
   fprintf(fptr, "Ceci est un exemple de texte écrit dans un fichier en C.");

   // fermer le fichier
   fclose(fptr);

   return 0;
}
```

En exécutant ce code, un fichier texte nommé "mon_fichier.txt" sera créé et un message sera écrit à l'intérieur.

## Dive Deep
Il est important de noter que lorsque vous écrivez un fichier texte en C, le fichier existant sera écrasé s'il en existe déjà un avec le même nom. De plus, il est recommandé d'utiliser le mode "a" plutôt que "w" pour écrire à la fin du fichier plutôt que de le remplacer entièrement.

Il existe également d'autres fonctions que vous pouvez utiliser en combinaison avec `fprintf()` pour écrire des données dans un fichier texte, telles que `fputc()` et `fputwc()`. Ces fonctions vous permettent d'écrire un caractère ou un caractère large à la fois dans le fichier.

## Voir aussi
Si vous souhaitez en savoir plus sur la manipulation des fichiers en C, consultez les ressources suivantes :

- [Documentation officielle de C](https://en.cppreference.com/w/c/language)
- [Tutoriel sur les fichiers en C](https://www.programiz.com/c-programming/c-file-input-output)
- [Exemple de manipulation de fichiers en C](https://www.studytonight.com/c/file-handling-in-c.php)

N'oubliez pas de pratiquer l'écriture de fichiers en C et de découvrir toutes les possibilités qu'elle offre pour vos projets de programmation !