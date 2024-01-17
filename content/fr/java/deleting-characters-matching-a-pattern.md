---
title:                "Suppression des caractères correspondant à un motif"
html_title:           "Java: Suppression des caractères correspondant à un motif"
simple_title:         "Suppression des caractères correspondant à un motif"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/java/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est et pourquoi le faire?

Supprimer des caractères correspondant à un modèle dans une chaîne de caractères est une tâche courante pour les programmeurs Java. Cela implique de supprimer toutes les occurrences d'un certain motif dans une chaîne donnée. Les programmeurs le font souvent pour nettoyer les données ou pour trouver et remplacer des mots spécifiques dans une chaîne.

## Comment faire:

Voici un exemple de code qui utilise la méthode `replaceAll` pour supprimer tous les caractères de ponctuation d'une chaîne de caractères donnée:

```Java
String text = "Bonjour, je vais dire bonjour à tout le monde!";
String nouvelleTexte = texte.replaceAll("[\\p{Punct}]", "");
System.out.println(nouvelleTexte);
```

Sortie:

```
Bonjour je vais dire bonjour à tout le monde
```

## Plongée en profondeur:

Bien qu'il existe de nombreuses façons de supprimer des caractères correspondant à un motif en Java, l'utilisation de la méthode `replaceAll` avec des expressions régulières est l'une des plus courantes. Cette méthode a été ajoutée dans la version 1.4 de Java et permet aux programmeurs de définir plus facilement le motif qu'ils souhaitent trouver et remplacer.

Il existe également d'autres méthodes telles que `replace` et `replaceAll(String,String)` qui peuvent être utilisées pour supprimer des caractères correspondant à un motif dans une chaîne de caractères. Cependant, ces méthodes sont moins flexibles car elles ne prennent pas en compte les expressions régulières.

Pour supprimer des caractères correspondant à un motif spécifique, les programmeurs peuvent également utiliser des bibliothèques tierces telles que Apache Commons StringUtils ou Guava CharMatcher. Celles-ci offrent une plus grande variété d'options pour supprimer des caractères dans une chaîne de caractères.

## Voir aussi:

- [Documentation Java: `String.replaceAll()`](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html#replaceAll-java.lang.String-java.lang.String-)
- [Tutoriel sur les expressions régulières Java](https://www.tutorialspoint.com/java/java_regular_expressions.htm)
- [Apache Commons StringUtils](https://commons.apache.org/proper/commons-lang/apidocs/org/apache/commons/lang3/StringUtils.html)
- [Guava CharMatcher](https://google.github.io/guava/releases/snapshot-jre/api/docs/com/google/common/base/CharMatcher.html)