---
title:                "Lecture des arguments de ligne de commande"
html_title:           "C: Lecture des arguments de ligne de commande"
simple_title:         "Lecture des arguments de ligne de commande"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous êtes un programmeur en herbe ou un expert en informatique, vous savez probablement déjà que les arguments de ligne de commande sont un aspect essentiel de la programmation en C. Cela permet aux utilisateurs de passer des données à votre programme au moment de l'exécution, vous offrant ainsi une plus grande flexibilité et une meilleure interaction avec les utilisateurs finaux. Dans cet article, nous allons expliquer pourquoi il est important de maîtriser la lecture des arguments de ligne de commande en C et comment le faire efficacement.

## Comment faire

La syntaxe pour lire les arguments de ligne de commande en C est assez simple. Tout d'abord, vous devez inclure la bibliothèque `stdio.h` en haut de votre fichier source. Ensuite, vous devez déclarer les paramètres `argc` (count) et `argv` (vector) dans votre fonction `main`, comme ceci :

```C
#include <stdio.h>

int main(int argc, char* argv[]) {
  // votre code ici
  return 0;
}
```

La variable `argc` contient le nombre total d'arguments passés à votre programme, tandis que `argv` est un pointeur vers un tableau de chaînes qui contient les arguments eux-mêmes. Maintenant, voyons un exemple concret pour mieux comprendre.

Supposons que vous avez un programme appelé `mon_programme` qui accepte deux arguments de ligne de commande : un nom suivi d'un âge. Vous pouvez utiliser `argc` pour vérifier si un nombre suffisant d'arguments a été passé et `argv` pour accéder aux valeurs de ces arguments, comme ceci :

```C
#include <stdio.h>

int main(int argc, char* argv[]) {
  if (argc < 3) {
    // gestion des erreurs si pas assez d'arguments sont passés
    printf("Usage: mon_programme <nom> <age>\n");
    return 1;
  }

  printf("Bonjour %s, vous avez %s ans !\n", argv[1], argv[2]);

  return 0;
}
```

Si vous invoquez ce programme en utilisant `./mon_programme John 25` dans le terminal, la sortie sera `Bonjour John, vous avez 25 ans !`. Mais si vous omettez un ou les deux arguments, le programme affichera le message d'erreur.

## Deep Dive

Maintenant que vous connaissez les bases de la lecture des arguments de ligne de commande en C, voici quelques informations plus avancées que vous pourriez trouver utiles :

- En utilisant `getopt()` ou `getopt_long()` de la bibliothèque `unistd.h`, vous pouvez facilement gérer les options de ligne de commande telles que `-h` ou `--help` pour afficher de l'aide ou d'autres fonctions spéciales. 
- Vous pouvez également utiliser la fonction `atoi()` de `stdlib.h` pour convertir une chaîne en entier lorsque vous lisez des arguments qui sont censés être des nombres.
- Pour gérer les arguments qui contiennent des espaces ou des caractères spéciaux, vous pouvez utiliser `strcpy()` ou `strncpy()` pour copier la chaîne entière du tableau `argv`.

## Voir aussi

- [La documentation officielle de C](https://en.cppreference.com/w/c/language/main_function)
- [Un guide complet sur les arguments de ligne de commande en C](https://www.gnu.org/software/libc/manual/html_node/Getopt-Long-Option-Example.html#Getopt-Long-Option-Example)
- [Un tutoriel vidéo sur la lecture des arguments de ligne de commande en C](https://www.youtube.com/watch?v=S64c_nPWt3I)