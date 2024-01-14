---
title:                "C: Lecture des arguments de ligne de commande"
programming_language: "C"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Pourquoi

Les arguments de ligne de commande sont un élément important dans la programmation en C. Ils permettent à un programme d'accepter des entrées de l'utilisateur directement à partir de la ligne de commande. Cela peut être utile pour des interactions rapides et pour faire des ajustements spécifiques lors de l'exécution du programme.

## Comment faire

Voici un exemple simple de lecture d'arguments de ligne de commande en utilisant la fonction `main` :

```C
int main(int argc, char *argv[]) {
  printf("Le nombre d'arguments est %d\n", argc);
  for (int i = 0; i < argc; i++) {
    printf("Argument %d : %s\n", i, argv[i]);
  }
  return 0;
}
```

Pour compiler et exécuter ce programme, utilisez les commandes suivantes :

```bash
gcc main.c -o main
./main argument1 argument2 argument3
```

Cela affichera le nombre d'arguments et chaque argument donné sur la ligne de commande. Dans cet exemple, nous avons trois arguments : argument1, argument2 et argument3.

## Plongée en profondeur

Maintenant que nous avons vu un exemple simple de lecture d'arguments de ligne de commande, il est temps de plonger un peu plus en profondeur. En plus de pouvoir accéder à chaque argument en tant qu'une chaîne de caractères, nous pouvons également utiliser des fonctions comme `atoi` ou `atof` pour convertir ces valeurs en types de données entiers ou flottants. De plus, nous pouvons spécifier des options pour les arguments en utilisant certaines bibliothèques externes comme `getopt`. Cela nous permet de créer des programmes plus avancés avec des fonctionnalités spécifiques et des options pour les utilisateurs.

## Voir aussi

- [Documentation officielle de la fonction `main`](https://en.cppreference.com/w/c/language/main_function)
- [Guide complet sur la lecture des arguments de ligne de commande en C](https://www.geeksforgeeks.org/command-line-arguments-in-c-cpp/)