---
title:    "Java: Lecture des arguments de ligne de commande"
keywords: ["Java"]
---

{{< edit_this_page >}}

##Pourquoi

Pour les programmeurs Java, il est très utile de savoir comment lire les arguments de ligne de commande afin de créer des programmes plus interactifs et flexibles. En utilisant cette fonctionnalité, les utilisateurs peuvent entrer des paramètres lorsqu'ils exécutent le code, ce qui permet de personnaliser l'exécution du programme.

##Comment faire

Pour lire les arguments de ligne de commande en Java, il existe deux méthodes principales : en utilisant la classe Scanner ou en utilisant l'argument "args" de la méthode main(). Voici un exemple de code pour chaque méthode :

```Java
// Utilisation de la classe Scanner
import java.util.Scanner;
public class ArgumentsScanner {
    public static void main(String[] args) {
        Scanner scanner = new Scanner(System.in);
        System.out.println("Entrez votre nom : ");
        String name = scanner.nextLine();
        System.out.println("Bonjour " + name + " !");
    }
}
```

```Java
// Utilisation de l'argument "args" de la méthode main()
public class ArgumentsMain {
    public static void main(String[] args) {
        System.out.println("Bonjour " + args[0] + " !");
    }
}
```

Pour utiliser la deuxième méthode, vous devez spécifier les arguments après le nom de la classe lors de l'exécution du code. Par exemple, si le code ci-dessus est compilé et exécuté avec l'argument "Alex", la sortie sera "Bonjour Alex !".

##Plongée en profondeur

Il est également possible de manipuler les arguments de ligne de commande de manière plus approfondie. Par exemple, vous pouvez vérifier le nombre d'arguments entrés en utilisant "args.length", ou encore convertir un argument en un type de données spécifique en utilisant des méthodes telles que "Integer.parseInt()". Les arguments peuvent également être utilisés pour effectuer des tâches spécifiques dans le programme, tels que des opérations mathématiques ou l'accès à des fichiers.

##Voir aussi

Pour plus d'informations sur la manipulation des arguments de ligne de commande en Java, voici quelques liens utiles :

- [Documentation officielle Java sur les arguments de ligne de commande](https://docs.oracle.com/javase/tutorial/essential/environment/cmdLineArgs.html)
- [Tutoriel vidéo sur la lecture des arguments de ligne de commande en Java](https://www.youtube.com/watch?v=WTyQNUuqgmo)
- [Exemples de codes pratiques pour la manipulation des arguments de ligne de commande](https://www.baeldung.com/java-command-line-arguments)