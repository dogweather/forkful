---
title:                "C: Vérifier si un répertoire existe"
simple_title:         "Vérifier si un répertoire existe"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Pourquoi

Dans le monde de la programmation, il est souvent nécessaire de travailler avec des fichiers et des répertoires. Il peut arriver qu'on ait besoin de vérifier si un répertoire existe avant de faire d'autres opérations telles que la création ou la suppression de fichiers. Dans cet article, nous allons explorer comment vérifier si un répertoire existe en utilisant le langage de programmation C.

## Comment faire

La première étape pour vérifier si un répertoire existe en C est de déclarer une variable de type `DIR` qui représente le répertoire. Ensuite, nous utilissons la fonction `opendir()` pour ouvrir le répertoire et stocker son pointeur dans la variable que nous avons déclarée. Si le répertoire existe, la fonction renverra un pointeur valide.

```C
DIR *rep = opendir("chemin/du/répertoire");
if (rep != NULL) {
  // Le répertoire existe, effectuer des opérations
} else {
  // Le répertoire n'existe pas, gérer l'erreur
}
```

Ensuite, pour libérer les ressources et fermer le répertoire, nous utilisons la fonction `closedir()` en passant le pointeur de notre répertoire en paramètre.

```C
closedir(rep); // Ferme le répertoire
```

## Plongée en profondeur

Pour une vérification plus précise, nous pouvons utiliser la fonction `opendir()` en passant le chemin du répertoire en paramètre. En cas de succès, cette fonction renvoie un pointeur valide et en cas d'erreur, elle renvoie NULL. Nous pouvons également utiliser la fonction `errno` pour obtenir la raison de l'échec et traiter l'erreur en conséquence.

Il est également important de noter que lorsqu'on travaille avec des répertoires, nous devons nous assurer que le code est portable sur différents systèmes d'exploitation. Ainsi, il est recommandé d'utiliser les fonctions de la bibliothèque standard C tels que `opendir()` et `closedir()` plutôt que des fonctions spécifiques à un système d'exploitation.

## Voir aussi

- [Documentation officielle sur la fonction opendir de la bibliothèque standard C](https://en.cppreference.com/w/c/io/opendir)
- [Utilisation de errno en programmation C](https://www.cs.utah.edu/~germain/PPS/Topics/C_Language/exceptions_and_error_handling.html#errno)