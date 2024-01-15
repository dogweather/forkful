---
title:                "Vérifier si un répertoire existe"
html_title:           "C: Vérifier si un répertoire existe"
simple_title:         "Vérifier si un répertoire existe"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous programmez en C, il est important de savoir comment vérifier si un répertoire existe. Cela peut être utile pour s'assurer que vous ne manipulez pas de fichiers ou de dossiers inexistants, évitant ainsi les erreurs et les bogues potentielles.

## Comment faire

Pour vérifier si un répertoire existe en C, vous pouvez utiliser la fonction `opendir()`. Cette fonction retourne un pointeur vers une structure de type `DIR` qui représente le répertoire spécifié. Si le répertoire n'existe pas, `opendir()` renvoie un pointeur nul. Voici un exemple de code utilisant `opendir()` pour vérifier si un répertoire existe :

```C
#include <stdio.h>
#include <dirent.h>

int main()
{
    // Déclarer un pointeur vers une structure DIR
    DIR *rep;

    // Spécifier le chemin vers le répertoire à vérifier
    char path[] = "./monDossier";

    // Utilisation de la fonction opendir() pour vérifier si le répertoire existe
    rep = opendir(path);

    // Vérification si opendir() a renvoyé un pointeur nul
    if (rep == NULL)
    {
        printf("Le répertoire n'existe pas.\n");
    }
    else
    {
        printf("Le répertoire existe.\n");
        // N'oubliez pas de fermer le pointeur vers le répertoire
        closedir(rep);
    }

    return 0;
}
```

Voici un exemple de sortie pour un répertoire existant :

```
Le répertoire existe.
```

Et pour un répertoire inexistant :

```
Le répertoire n'existe pas.
```

## Plongée en profondeur

Bien que `opendir()` soit la fonction la plus couramment utilisée pour vérifier l'existence d'un répertoire en C, il existe également d'autres fonctions pouvant être utiles dans certaines situations. Par exemple, la fonction `access()` peut être utilisée pour vérifier non seulement l'existence d'un répertoire, mais aussi si l'utilisateur a les droits nécessaires pour y accéder. De plus, la fonction `stat()` peut être utilisée pour récupérer des informations spécifiques sur le répertoire, telles que sa taille ou sa date de création.

Il est également important de noter que la vérification de l'existence d'un répertoire en C peut dépendre du système d'exploitation sur lequel le code est exécuté. Par conséquent, il peut être utile de consulter la documentation de votre système d'exploitation pour obtenir des informations plus détaillées sur les fonctions à utiliser pour vérifier l'existence d'un répertoire.

## Voir aussi

- [Documentation `opendir()` sur cppreference.com](https://en.cppreference.com/w/c/io/opendir)
- [Documentation `access()` sur cppreference.com](https://en.cppreference.com/w/c/io/access)
- [Documentation `stat()` sur cppreference.com](https://en.cppreference.com/w/c/io/stat)