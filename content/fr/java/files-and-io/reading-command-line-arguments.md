---
date: 2024-01-20 17:56:10.102794-07:00
description: "How to: La magie op\xE8re dans la signature de la m\xE9thode `main`.\
  \ Voil\xE0 comment \xE7a marche ."
lastmod: '2024-03-13T22:44:57.658966-06:00'
model: gpt-4-1106-preview
summary: "La magie op\xE8re dans la signature de la m\xE9thode `main`."
title: Lecture des arguments de ligne de commande
weight: 23
---

## How to:
La magie opère dans la signature de la méthode `main`. Voilà comment ça marche :

```java
public class CommandLineExample {
    public static void main(String[] args) {
        if (args.length > 0) {
            System.out.println("Voici les arguments de ligne de commande que vous avez passés :");
            for (String arg : args) {
                System.out.println(arg);
            }
        } else {
            System.out.println("Aucun argument n'a été passé !");
        }
    }
}
```

Compilez et exécutez avec différents arguments :

```shell
javac CommandLineExample.java
java CommandLineExample Bonjour à tous!
```

Sortie :

```
Voici les arguments de ligne de commande que vous avez passés :
Bonjour
à
tous!
```

## Deep Dive
Avant, en C par exemple, lire des arguments était plus complexe. Java l'a simplifié en utilisant le tableau `args` de `main`. Il y a d'autres moyens, comme les bibliothèques `Apache Commons CLI` ou `JCommander`, pour gérer des cas plus complexes ou pour interpréter des options spécifiques. Techniquement, `args` est un tableau de `String`, le système d'exploitation découpe la ligne de commande en segments et Java les transmet à votre application.

## See Also
Pour aller plus loin :

- Documentation Oracle sur les arguments de ligne de commande : [Oracle Docs](https://docs.oracle.com/javase/tutorial/essential/environment/cmdLineArgs.html)
- Apache Commons CLI pour une gestion avancée des arguments : [Apache Commons CLI](https://commons.apache.org/proper/commons-cli/)
- Guide d'utilisation de JCommander : [JCommander](http://jcommander.org/)
