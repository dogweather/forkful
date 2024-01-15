---
title:                "Recherche et remplacement de texte"
html_title:           "Java: Recherche et remplacement de texte"
simple_title:         "Recherche et remplacement de texte"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/java/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Pourquoi
Alors, vous vous demandez pourquoi vous devriez vous intéresser à la recherche et au remplacement de texte en Java ? Eh bien, c'est une tâche courante pour les développeurs lorsqu'ils travaillent avec des chaînes de caractères dans leurs programmes. Cela peut vous faire gagner du temps et éviter les erreurs manuelles.

## Comment faire
Pour effectuer une recherche et un remplacement de texte en Java, vous pouvez utiliser la méthode `replace()` de la classe `String`. Elle prend en paramètres une chaîne de caractères à remplacer et la nouvelle chaîne à laquelle elle doit être remplacée. Voyons un exemple concret :

```
String phrase = "Bonjour tout le monde";
String nouvellePhrase = phrase.replace("tout", "à tous");
System.out.println(nouvellePhrase);
```
Résultat : "Bonjour à tous le monde"

Vous pouvez également utiliser la méthode `replaceAll()` pour remplacer toutes les occurrences d'une certaine chaîne de caractères dans une phrase. Dans cet exemple, nous allons remplacer tous les "l" par des "y" dans la phrase :

```
String phrase = "Hello World";
String nouvellePhrase = phrase.replaceAll("l", "y");
System.out.println(nouvellePhrase);
```
Résultat : "Heyyo Woryd"

## Plongée en profondeur
Il est important de noter que les méthodes `replace()` et `replaceAll()` ne modifient pas la chaîne originale, elles renvoient plutôt une nouvelle chaîne avec les modifications effectuées. De plus, vous pouvez utiliser des expressions régulières pour rechercher des motifs plus complexes dans une chaîne de caractères lors du remplacement. Vous pouvez également utiliser des méthodes de la classe `StringBuilder` pour modifier une chaîne sans en créer une nouvelle chaque fois.

## Voir aussi
- [Documentation officielle Java sur la classe String](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html)
- [Tutoriel de recherche et de remplacement de texte en Java](https://www.baeldung.com/java-replace-string)
- [Guide complet sur les expressions régulières en Java](https://www.regular-expressions.info/java.html)