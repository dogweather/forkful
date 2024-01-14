---
title:                "C: La lecture d'un fichier texte"
programming_language: "C"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Pourquoi

Lire un fichier texte est une tâche courante en programmation, que ce soit pour récupérer des données ou les afficher à l'utilisateur. Dans cet article, nous allons voir comment lire un fichier texte en utilisant le langage de programmation C.

## Comment faire

Pour lire un fichier texte en C, nous allons utiliser la fonction `fopen` pour ouvrir le fichier et `fgets` pour lire ligne par ligne. Voici un exemple de code :

```C
#include <stdio.h>

int main(){
    // Ouverture du fichier en mode lecture
    FILE* fichier = fopen("mon_fichier.txt", "r");

    // Boucle pour lire chaque ligne du fichier
    char ligne[100];
    while (fgets(ligne, 100, fichier) != NULL) {
        // Affichage de la ligne lue à l'utilisateur
        printf("%s", ligne);
    }

    // Fermeture du fichier
    fclose(fichier);

    return 0;
}
```

Supposons que notre fichier texte `mon_fichier.txt` contienne les lignes suivantes :

```
Bonjour,
Comment ça va ?
```

Lorsque nous exécutons le code précédent, nous obtiendrons en sortie :

```
Bonjour,
Comment ça va ?
```

Comme vous pouvez le constater, la boucle `while` lit chaque ligne du fichier et utilise `printf` pour l'afficher à l'utilisateur.

## Plongée en profondeur

Maintenant que nous avons vu un exemple basique de lecture de fichier texte en C, voyons quelques informations plus détaillées sur la manipulation de fichiers.

- La fonction `fopen` prend deux paramètres : le nom du fichier et le mode d'ouverture. Le mode `r` indique que le fichier doit être ouvert en lecture seule. Vous pouvez également utiliser `w` pour écrire dans un fichier ou `a` pour ajouter du contenu à la fin du fichier.
- Le premier paramètre de la fonction `fgets` est un pointeur vers un tableau de caractères qui contiendra la ligne lue. Le deuxième paramètre est la taille maximale de la ligne et le dernier paramètre est un pointeur vers le fichier. `fgets` renvoie `NULL` lorsque la fin du fichier est atteinte.
- N'oubliez pas de fermer le fichier après avoir fini de le lire en utilisant la fonction `fclose` pour éviter les fuites de mémoire.

Maintenant que vous avez les bases pour lire un fichier texte en C, vous pouvez explorer d'autres fonctions telles que `fgetc` pour lire un caractère à la fois ou `fseek` pour déplacer le curseur de lecture dans le fichier.

# Voir aussi

- [Documentation officielle de la fonction fopen en C](https://www.gnu.org/software/libc/manual/html_node/Opening-and-Closing-Streams.html)
- [Tutoriel sur la lecture de fichiers en C](https://www.programiz.com/c-programming/c-file-input-output)
- [Exemples avancés de manipulation de fichiers en C](https://www.guru99.com/c-file-handling.html)