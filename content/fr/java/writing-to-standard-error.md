---
title:                "Java: Écrire vers l'erreur standard"
programming_language: "Java"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/java/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Pourquoi

L'écriture sur la sortie d'erreur standard, également connue sous le nom de standard error, est une pratique courante en programmation Java. Cette méthode permet aux développeurs de gérer efficacement les erreurs et les exceptions dans leur code.

## Comment faire

Pour écrire sur la sortie d'erreur standard en Java, vous pouvez utiliser la méthode suivante:

```Java
System.err.println("Message d'erreur à afficher");
```

Cette méthode affichera le message d'erreur spécifié dans la console en couleur rouge, ce qui le distinguera du reste de la sortie standard.

Voici un exemple de sortie:

```
Erreur: Impossible de se connecter à la base de données
```

Il est également possible d'écrire directement sur la sortie d'erreur standard en utilisant un objet de type PrintStream, comme ceci:

```Java
PrintStream err = System.err;
err.println("Message d'erreur à afficher");
```

Cette méthode offre une flexibilité supplémentaire, car vous pouvez spécifier un emplacement différent pour le message d'erreur en utilisant la méthode `err.println()`.

## Analyse approfondie

L'écriture sur la sortie d'erreur standard est une pratique utile pour gérer les erreurs dans votre code Java. Cela permet de différencier clairement les messages d'erreur des autres sorties et facilite le débogage. De plus, cela aide les utilisateurs à comprendre pourquoi une erreur s'est produite lors de l'exécution de votre programme.

Il est important de noter que vous ne devriez utiliser la sortie d'erreur standard que pour les erreurs réelles et non pour afficher des messages de débogage ou de diagnostic. Pour ces derniers, utilisez plutôt la sortie standard avec `System.out.println()`. Cela évitera toute confusion lors de l'exécution du programme.

## Voir aussi

- [Documentation sur la classe PrintStream](https://docs.oracle.com/javase/8/docs/api/java/io/PrintStream.html)
- [Article sur l'utilisation de System.err.println() en Java](https://www.journaldev.com/19194/system-err-println-vs-system-out-println-in-java)

En utilisant correctement la sortie d'erreur standard dans votre code Java, vous pouvez améliorer la qualité et la lisibilité de votre programme. N'hésitez pas à l'utiliser pour gérer les erreurs et à consulter les liens ci-dessus pour en savoir plus sur cette pratique.