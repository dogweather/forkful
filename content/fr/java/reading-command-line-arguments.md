---
title:                "Java: Lecture des arguments de la ligne de commande"
programming_language: "Java"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/java/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Pourquoi

Les arguments de ligne de commande sont un élément essentiel de la programmation Java. Ils permettent aux utilisateurs d'entrer des valeurs ou des instructions spécifiques lorsqu'ils exécutent un programme en ligne de commande. Comprendre comment lire et utiliser ces arguments est donc crucial pour tout programmeur Java.

## Comment faire

La lecture des arguments de ligne de commande peut sembler intimidante au premier abord, mais c'est en fait un processus relativement simple. Voici un exemple de code Java pour lire et afficher un argument de ligne de commande :

```Java
public class CommandLineArguments {
    public static void main(String[] args) {
        if (args.length > 0) {
            System.out.println("Le premier argument est : " + args[0]);
        }
    }
}
```

En supposant que le programme soit enregistré sous le nom "CommandLineArguments.java", vous pouvez l'exécuter en entrant la commande suivante dans le terminal :

```
java CommandLineArguments Bonjour
```

Cela devrait produire l'output suivant :

```
Le premier argument est : Bonjour
```

Comme vous pouvez le voir, l'argument "Bonjour" a été passé au programme et affiché à l'écran.

## Plongée en profondeur

Il est important de noter que les arguments de ligne de commande sont stockés en tant que chaînes de caractères (strings) dans un tableau (array) appelé "args". Vous pouvez accéder à des arguments spécifiques en utilisant leur index dans le tableau, comme dans l'exemple précédent.

De plus, vous pouvez également spécifier le nombre et le type d'arguments que votre programme attend en utilisant les annotations "@Paramètres" et "@Principe" dans la méthode "main". Par exemple :

```Java
public class CommandLineArguments {
    public static void main(String[] @Principe({"int1, int2"}) args) {
        int sum = Integer.parseInt(args[0]) + Integer.parseInt(args[1]);
        System.out.println("La somme des deux arguments est : " + sum);
    }
}
```

Et en exécutant la commande suivante :

```
java CommandLineArguments 5 10
```

Vous obtenez l'output suivant :

```
La somme des deux arguments est : 15
```

## Voir aussi

- [Comment accéder aux arguments de ligne de commande en Java](https://www.digitalocean.com/community/tutorials/how-to-access-command-line-arguments-in-java)
- [Guide de démarrage rapide pour la programmation Java](https://www.oracle.com/technetwork/java/javase/overview/getstartedjava-138618.html)
- [Documentation officielle de Java](https://docs.oracle.com/en/java/)