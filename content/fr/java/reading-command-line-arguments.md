---
title:                "Java: Lire les arguments de la ligne de commande"
simple_title:         "Lire les arguments de la ligne de commande"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/java/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# Pourquoi

Les arguments de ligne de commande sont des éléments essentiels pour tout programmeur Java. C'est un moyen pratique de personnaliser l'exécution d'un programme en fournissant des valeurs spécifiques au démarrage. Dans cet article, nous allons plonger dans les raisons pour lesquelles il est important de savoir lire les arguments de ligne de commande.

# Comment faire

Tout d'abord, nous devons être familiers avec la méthode `main()` en Java, qui est la première méthode à être exécutée lorsqu'un programme démarre. Cette méthode prend un tableau de chaînes, qui sont les arguments de ligne de commande, en tant que paramètre. Pour récupérer ces arguments, nous pouvons utiliser la boucle `for` suivante :

```Java
public static void main(String[] args) {
    // Boucle pour parcourir les arguments
    for (String arg : args) {
        System.out.println(arg);
    }
}
```

Imaginons que nous exécutons notre programme avec les arguments "Bonjour" et "monde". La sortie sera :

```
Bonjour
monde
```

Nous pouvons également convertir ces arguments en entiers ou en d'autres types de données selon nos besoins, en utilisant simplement les méthodes correspondantes telles que `Integer.parseInt()`.

# Plongée en profondeur

Il est important de savoir lire les arguments de ligne de commande car cela nous donne une plus grande flexibilité dans l'exécution de nos programmes. Nous pouvons les utiliser pour fournir des paramètres de configuration, des entrées utilisateur ou d'autres informations dynamiques nécessaires à l'exécution de notre programme. Cela rend notre code plus dynamique et plus personnalisable.

Il est également utile de savoir comment envoyer des arguments de ligne de commande en utilisant l'invite de commandes ou l'environnement de développement intégré que nous utilisons. Cela nous permet de déboguer plus facilement notre code en fournissant des valeurs spécifiques pour tester différents scénarios.

# Voir aussi

- [Documentation officielle Java sur la méthode `main()`](https://docs.oracle.com/javase/tutorial/getStarted/application/index.html)
- [Tutoriel sur la lecture des arguments de ligne de commande en Java](https://www.tutorialspoint.com/java/java_command_line_arguments.htm)
- [Autre exemple d'utilisation des arguments de ligne de commande en Java](https://www.baeldung.com/java-command-line-arguments)