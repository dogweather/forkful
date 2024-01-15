---
title:                "Lecture des arguments en ligne de commande"
html_title:           "Java: Lecture des arguments en ligne de commande"
simple_title:         "Lecture des arguments en ligne de commande"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/java/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Pourquoi

L'utilisation des arguments de ligne de commande peut être très pratique pour les développeurs Java, car cela leur permet de passer des paramètres lors de l'exécution du programme sans avoir à les modifier dans le code source. Cela rend le programme plus flexible et plus facile à utiliser.

## Comment faire

Pour lire les arguments de ligne de commande en Java, il suffit d'utiliser l'objet `args` dans la méthode `main()` de votre classe. Voici un exemple de code pour illustrer cela :

```Java
public class CommandLineArguments {

    public static void main(String[] args) {

        System.out.println("Le nombre d'arguments passés est : " + args.length);

        for(int i = 0; i < args.length; i++) {
            System.out.println("Argument " + (i+1) + " : " + args[i]);
        }
    }
}
```

Si vous exécutez ce programme en passant des arguments de ligne de commande, vous obtiendrez un résultat similaire à ceci :

```
$ java CommandLineArguments Je suis un article
Le nombre d'arguments passés est : 4
Argument 1 : Je
Argument 2 : suis
Argument 3 : un
Argument 4 : article
```

Vous pouvez également utiliser des arguments de ligne de commande pour passer des valeurs numériques ou des chemins de fichiers. Pour les valeurs numériques, vous devrez les convertir en utilisant les méthodes appropriées, par exemple `Integer.parseInt()` pour les entiers. Pour les chemins de fichiers, vous pouvez utiliser la classe `File` pour créer un objet représentant le fichier spécifié.

## Plongée en profondeur

Lorsque vous lisez des arguments de ligne de commande, il est important de prendre en compte les différents cas d'utilisation et de gérer les erreurs possibles. Par exemple, que se passe-t-il si l'utilisateur ne passe aucun argument ? Ou s'il passe un argument invalide ? Vous devrez également être conscient de la longueur maximale des arguments, qui peut varier en fonction du système d'exploitation.

Il est également important de noter que la lecture des arguments de ligne de commande fait partie du niveau bas de programmation et qu'il existe des bibliothèques tierces qui peuvent faciliter cette tâche en offrant une API plus conviviale et en gérant certaines des complexités mentionnées ci-dessus.

## Voir aussi

- [Documentation Oracle sur les arguments de ligne de commande en Java](https://docs.oracle.com/javase/tutorial/essential/environment/cmdLineArgs.html)
- [Apache Commons CLI - une bibliothèque tiers pour faciliter la lecture des arguments de ligne de commande](https://commons.apache.org/proper/commons-cli/)