---
title:                "La lecture des arguments de ligne de commande"
html_title:           "Java: La lecture des arguments de ligne de commande"
simple_title:         "La lecture des arguments de ligne de commande"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/java/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# Qu'est-ce que c'est et pourquoi les programmeurs le font?

Lire les arguments de ligne de commande est une façon pour les programmeurs de donner des instructions spécifiques au programme sans avoir à les coder directement dans le code source. Cela permet une plus grande flexibilité et personnalisation pour les utilisateurs finaux du programme.

# Comment faire:

Voici un exemple simple de lecture des arguments de ligne de commande en Java:

```java
public class CommandLineArguments {
    public static void main(String[] args) {
        System.out.println("Le premier argument est: " + args[0]);
        System.out.println("Le deuxième argument est: " + args[1]);
    }
}
```

Supposons que nous exécutons ce programme avec les arguments suivants:

```bash
java MyProgramme arg1 arg2
```

La sortie sera:

```bash
Le premier argument est: arg1
Le deuxième argument est: arg2
```

# Plongée profonde:

Lire les arguments de ligne de commande existe depuis les débuts du langage Java. Cela permet aux utilisateurs de personnaliser les instructions pour le programme sans avoir à recompiler le code source. D'autres alternatives pour transmettre des instructions à un programme comprennent l'utilisation de fichiers de configuration ou d'interfaces graphiques.

La syntaxe pour lire les arguments de ligne de commande en Java est la suivante:

```java
public static void main(String[] args)
```

où ```args``` est un tableau de type ```String``` contenant les arguments de la ligne de commande passés par l'utilisateur lors de l'exécution du programme.

# Voir aussi:

Pour plus d'informations sur la lecture des arguments de ligne de commande en Java, consultez les liens suivants:

- [Documentation officielle Java: Lire les arguments de ligne de commande](https://docs.oracle.com/javase/tutorial/essential/environment/cmdLineArgs.html)
- [Définir des arguments de ligne de commande en Java](https://www.baeldung.com/java-command-line-arguments)
- [Exemples de code pour lire les arguments de ligne de commande en Java](https://www.tutorialspoint.com/java/java_command_line_arguments.htm)