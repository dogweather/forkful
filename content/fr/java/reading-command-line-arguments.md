---
title:                "Lecture des arguments de ligne de commande"
html_title:           "Ruby: Lecture des arguments de ligne de commande"
simple_title:         "Lecture des arguments de ligne de commande"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/java/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi?

Les arguments de la ligne de commande sont les entrées que vous donnez à votre programme lors de son exécution. Ils sont essentiels pour rendre votre programme modulable et interactif.

## Comment Faire:
```java
public class Main {

    public static void main(String[] args) {
        // Boucle à travers les arguments de la ligne de commande
        for(String str: args) {
            System.out.println(str);
        }
    }
}
```
Si vous exécutez ce programme avec `java Main bonjour monde`, l'output sera:
```
bonjour
monde
```

## Plongée en Profondeur

1. **Contexte historique**: L'utilisation des arguments de ligne de commande remonte aux débuts de l'informatique quand les interfaces graphiques n'existaient pas. Il est toujours en vigueur en raison de sa simplicité et efficacité.
   
2. **Alternatives**: Vous pouvez également utiliser BufferedReader ou Scanner pour lire les entrées, mais ils sont utilisés pour obtenir des entrées de l'utilisateur en cours d'exécution plutôt qu'au démarrage du programme.

3. **Détails d'implémentation**: Les arguments de la ligne de commande sont passés sous forme de tableau de string (String[]) à la méthode main(). Chaque chaîne dans le tableau correspond à un argument.

## Voir Aussi

- Oracle Java Docs: [Command-Line Arguments](https://docs.oracle.com/javase/tutorial/essential/environment/cmdLineArgs.html)

- StackOverflow: [How are parameters passed in Java main method?](https://stackoverflow.com/questions/890966/what-is-string-args-parameter-in-main-method-java)