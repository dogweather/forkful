---
title:                "C: Lecture des arguments de ligne de commande"
simple_title:         "Lecture des arguments de ligne de commande"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Pourquoi

Les arguments de ligne de commande sont un élément essentiel de la programmation en C et il est important de les comprendre pour réussir en tant que développeur. Lorsque vous comprenez comment lire les arguments de ligne de commande, vous pouvez créer des programmes plus flexibles et plus puissants.

## Comment faire

Pour lire les arguments de ligne de commande en C, vous devez utiliser la fonction `main()`. Voici un exemple de code qui illustre comment cela peut être fait:

```
#include <stdio.h>

int main(int argc, char *argv[]) {
    int i;

    // Boucle à travers les arguments de ligne de commande
    for(i = 0; i < argc; i++) {
        // Afficher chaque argument
        printf("Argument %d : %s\n", i, argv[i]);
    }

    return 0;
}
```

Dans cet exemple, la variable `argc` contient le nombre total d'arguments passés à votre programme et la variable `argv[]` contient une liste de ces arguments sous forme de chaînes de caractères. Le premier argument est toujours le nom du programme lui-même.

Pour tester ce code, vous pouvez le compiler et exécuter avec différents arguments en ligne de commande. Par exemple, si vous nommez votre programme "programme", vous pouvez exécuter les commandes suivantes:

```
$ ./programme
```
Ce qui donne la sortie suivante:

```
Argument 0 : programme
```
Et si vous exécutez la commande suivante:

```
$ ./programme argument1 argument2 argument3
```

La sortie sera:

```
Argument 0 : programme
Argument 1 : argument1
Argument 2 : argument2
Argument 3 : argument3
```

## Plongée en profondeur

Il est important de noter que toutes les chaînes de caractères reçues en tant qu'arguments de ligne de commande sont stockées en mémoire en continu. Cela signifie que si vous avez besoin de modifier une chaîne, vous devrez créer une copie en mémoire pour éviter toute modification de l'argument original.

Il est également possible d'utiliser des arguments en option en utilisant le caractère de tiret "-". Par exemple, si vous exécutez votre programme avec les arguments "-a" et "-b", vous pouvez utiliser le code suivant pour vérifier quelles options ont été utilisées:

```
// Vérifie si l'argument "-a" a été utilisé
if(strcmp(argv[i], "-a") == 0) {
    // Effectuez une action ...
}
```

## Voir aussi

Maintenant que vous comprenez comment lire les arguments de ligne de commande en C, vous pouvez explorer d'autres fonctionnalités de la ligne de commande en utilisant les liens suivants:

- [La ligne de commande en C sur OpenClassrooms](https://openclassrooms.com/fr/courses/19980-apprenez-a-programmer-en-c/17190-la-ligne-de-commande-en-c)
- [Gestion des erreurs de la ligne de commande en C sur Stack Overflow](https://stackoverflow.com/questions/8111673/dealing-with-command-line-errors-in-c)
- [Passage d'une structure en ligne de commande en C sur GeeksforGeeks](https://www.geeksforgeeks.org/passing-structure-command-line-arguments-c/)