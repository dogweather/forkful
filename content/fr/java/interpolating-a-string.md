---
title:                "Interpolation d'une chaîne de caractères"
html_title:           "Ruby: Interpolation d'une chaîne de caractères"
simple_title:         "Interpolation d'une chaîne de caractères"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/java/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Qu'est-ce et pourquoi?

L'interpolation de chaînes en Java permet d'insérer des variables directement dans une chaîne de caractères. C'est utile pour rendre le code plus lisible et pour générer des messages de log dynamiques.

## Comment faire:

Voici un exemple simple utilisant `String.format()` :
```Java
String name = "John";
String greeting = String.format("Bonjour, %s!", name);
System.out.println(greeting);  // Affiche: Bonjour, John!
```
Notez l'utilisation de `%s` comme marqueur de place pour la variable `name`.

On peut aussi utiliser `MessageFormat.format()` pour obtenir le même résultat :

```Java
import java.text.MessageFormat;

String name = "John";
String greeting = MessageFormat.format("Bonjour, {0}!", name);
System.out.println(greeting);  // Affiche: Bonjour, John!
```

Ici, {0} est un index basé sur zéro qui pointe vers le premier argument dans la liste de paramètres.

## Plongée profonde

Historiquement, l'interpolation de chaînes a été introduite en C avec `printf()`. Elle a été adoptée par de nombreux autres langages, chacun avec sa propre implémentation.

En ce qui concerne les alternatives en Java, nous avons également la concaténation de chaînes avec `+`, mais elle peut être moins performante et moins lisible pour les chaînes longues ou complexes.

Sous le capot, `String.format()` et `MessageFormat.format()` utilisent les classes `java.util.Formatter` et `java.text.MessageFormat` respectivement. Ces dernières utilisent un buffer pour construire la chaîne de caractères, ce qui est plus efficace que la concaténation simple.

## Voir aussi

- API Java Docs pour [String.format()](https://docs.oracle.com/en/java/javase/15/docs/api/java.base/java/lang/String.html#format(java.lang.String,java.lang.Object...)) et [MessageFormat.format()](https://docs.oracle.com/en/java/javase/15/docs/api/java.base/java/text/MessageFormat.html#format(java.lang.Object))
- Comparaison de performance entre `String.format()` et la concaténation de chaînes sur [Stack Overflow](https://stackoverflow.com/questions/513600/should-i-use-java-string-format-if-performance-is-important)