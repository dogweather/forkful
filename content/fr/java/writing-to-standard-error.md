---
title:    "Java: Écrire vers l'erreur standard"
keywords: ["Java"]
---

{{< edit_this_page >}}

## Pourquoi

Écrire vers la sortie d'erreur standard est un moyen utile pour le débogage et la gestion des erreurs dans votre code Java. Cela permet de séparer les messages d'erreur des messages de sortie pour une meilleure lisibilité lors de l'exécution de votre programme.

## Comment faire

Pour écrire vers la sortie d'erreur standard en Java, il suffit d'utiliser la méthode "System.err.println()" et de lui passer en paramètre le message que vous souhaitez afficher. Voici un exemple de code :

```Java
System.err.println("Erreur : impossible de lire le fichier");
```

Lors de l'exécution de votre programme, le message "Erreur : impossible de lire le fichier" sera affiché dans la sortie d'erreur standard.

## Plongée en profondeur

Il est important de noter que la sortie d'erreur standard n'est pas la même que la sortie standard (System.out). La sortie d'erreur standard est destinée à afficher uniquement les messages d'erreur, alors que la sortie standard est réservée pour les messages de sortie normaux. Vous pouvez également utiliser la méthode "System.err.print()" pour afficher un message sans retour à la ligne, contrairement à la méthode "println()" qui ajoute automatiquement un retour à la ligne.

Il est également possible de rediriger la sortie d'erreur standard vers un fichier en utilisant la commande ">" dans le terminal, par exemple :

```
java MonProgramme > fichiersortie.txt
```

Cela permet de stocker les messages d'erreur dans un fichier pour une consultation ultérieure.

## Voir aussi

- [Documentation officielle de Java sur la sortie d'erreur standard](https://docs.oracle.com/javase/tutorial/essential/io/file.html)
- [Guide complet sur la gestion des erreurs en Java](https://www.baeldung.com/java-exceptions)
- [Tutoriel sur la redirection de la sortie d'erreur standard en Java](https://www.geeksforgeeks.org/redirecting-system-out-println-output-to-a-file-in-java/)