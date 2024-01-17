---
title:                "Écriture vers l'erreur standard"
html_title:           "Java: Écriture vers l'erreur standard"
simple_title:         "Écriture vers l'erreur standard"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/java/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Qu'est-ce que la sortie d'erreur standard?
La sortie d'erreur standard, également connue sous le nom de stderr, est un canal utilisé dans la programmation pour afficher des messages d'erreur. Cela permet aux développeurs de détecter et de diagnostiquer rapidement les problèmes dans leur code.

Pourquoi les programmeurs utilisent-ils la sortie d'erreur standard?
En tant que programmeurs, il est essentiel de connaître les erreurs qui se produisent dans notre code afin de pouvoir les corriger. En guidant les erreurs vers la sortie d'erreur standard, nous pouvons facilement les identifier et les résoudre. Cela permet également aux développeurs de fournir des informations sur les erreurs aux utilisateurs de leur programme.

## Comment faire:
```Java
try {
  // code pouvant potentiellement causer une erreur
} catch(Exception e) {
  System.err.println(e); // envoie l'erreur à la sortie d'erreur standard
}
```
Exemple de sortie:
```
java.lang.NullPointerException
    at MyClass.myMethod(MyClass.java:12)
    ...
```
Dans cet exemple, la sortie affiche l'erreur qui s'est produite (NullPointerException) et où elle s'est produite (ligne 12 de la méthode myMethod dans la classe MyClass).

## Plongée en profondeur:
La sortie d'erreur standard a été introduite pour la première fois dans le langage de programmation C. De nos jours, elle est couramment utilisée dans de nombreux langages, dont Java. Les alternatives à la sortie d'erreur standard incluent la journalisation et l'utilisation de bibliothèques spécifiques à la gestion des erreurs.

Les messages d'erreur affichés sur la sortie d'erreur standard peuvent être personnalisés en utilisant la méthode ```System.err.print(message)``` ou ```System.err.println(message)```. Nous pouvons également rediriger la sortie d'erreur standard vers un fichier en utilisant la classe ```java.util.logging```.

## Voir aussi:
- [La documentation officielle de Java sur la sortie d'erreur standard](https://docs.oracle.com/javase/8/docs/api/java/lang/System.html#err)
- [Un guide sur la gestion des erreurs en Java](https://www.baeldung.com/java-exceptions)
- [Une explication détaillée de la journalisation en Java](https://stackify.com/guide-logging-java/)