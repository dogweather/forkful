---
title:    "Java: La lecture des arguments en ligne de commande"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/java/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Pourquoi

Les arguments de ligne de commande sont une partie importante de la programmation Java. Ils permettent aux utilisateurs de fournir des informations spécifiques à un programme lors de son exécution. Comprendre comment lire ces arguments est essentiel pour créer des programmes flexibles et dynamiques.

## Comment faire

Il existe plusieurs façons de lire les arguments de ligne de commande en Java. La méthode la plus courante consiste à utiliser la classe `Scanner` pour lire les entrées de la console. Voici un exemple de code avec une sortie d'échantillon :

```Java
import java.util.Scanner;

public class CommandLineArgs {
    public static void main(String[] args) {
        Scanner scanner = new Scanner(System.in);
        System.out.println("Veuillez entrer votre nom : ");
        String name = scanner.nextLine();
        System.out.println("Bonjour " + name + ", bienvenue sur notre programme !");
    }
}
```

Sortie :

```
Veuillez entrer votre nom : John
Bonjour John, bienvenue sur notre programme !
```

Une autre méthode consiste à utiliser l'objet `args` dans la méthode `main` pour récupérer les arguments. Voici un exemple :

```Java
public class CommandLineArgs {
    public static void main(String[] args) {
        System.out.println("Bonjour " + args[0] + ", bienvenue sur notre programme !");
    }
}
```

Sortie :

```
Bonjour John, bienvenue sur notre programme !
```

## Plongée profonde

Maintenant que vous savez comment lire les arguments de ligne de commande en Java, il est important de comprendre quelques points essentiels. Premièrement, les arguments sont stockés sous forme de chaînes de caractères, vous devrez donc les convertir en types appropriés si nécessaire. Deuxièmement, les arguments peuvent être fournis dans n'importe quel ordre, il est donc important de les gérer correctement dans votre code.

Il est également important de noter que les arguments peuvent contenir des options, comme dans les commandes de type `git commit -m "message"` où `commit` est l'option et `-m` et `"message"` sont les arguments. Pour lire ces options, vous devrez utiliser une bibliothèque ou écrire votre propre analyseur d'arguments.

## Voir aussi

- [Documentation officielle de Java sur les arguments de ligne de commande](https://docs.oracle.com/javase/tutorial/essential/environment/cmdLineArgs.html)
- [Article de TutorielsPoint sur les arguments de ligne de commande en Java](https://www.tutorialspoint.com/java_programming/java_command_line_arguments.htm)
- [Guide de programmation Java - Les arguments de ligne de commande](https://www.java.com/fr/download/help/develop.html#args)