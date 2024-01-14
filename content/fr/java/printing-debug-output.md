---
title:                "Java: Affichage des sorties de débogage"
programming_language: "Java"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/java/printing-debug-output.md"
---

{{< edit_this_page >}}

# Pourquoi

Imprimer des informations de débogage peut sembler fastidieux et inutile au premier abord, mais c'est en réalité un outil précieux pour tout programmeur Java. Cela permet de comprendre et de résoudre les erreurs qui peuvent survenir lors de l'exécution de notre code.

# Comment faire

Utiliser la méthode "println" est la façon la plus courante d'imprimer du contenu de débogage. Voici un exemple simple pour afficher le contenu d'une variable "i" :

```Java
int i = 5;
System.out.println("La valeur de i est : " + i);
```

Cela affichera dans la console : "La valeur de i est : 5". Nous pouvons également utiliser la méthode "print" si nous ne souhaitons pas ajouter de saut de ligne à la fin.

# Plongez plus profondément

Au-delà de la méthode "println", il existe d'autres moyens d'imprimer des informations de débogage. Par exemple, nous pouvons utiliser la bibliothèque "java.util.logging" pour enregistrer des messages de débogage ou encore utiliser des outils tels que "System.err" pour afficher des erreurs. De plus, nous pouvons également formater nos sorties en utilisant la classe "String.format".

# Voir aussi

- [Guide de débogage Java pour débutants](https://www.tutorialspoint.com/java/java_debugging.htm)
- [Utiliser java.util.logging pour déboguer](https://www.baeldung.com/java-util-logging)
- [Documentation officielle Java sur la classe String](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html)