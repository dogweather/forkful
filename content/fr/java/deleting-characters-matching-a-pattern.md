---
title:                "Supprimer les caractères correspondants à un motif"
html_title:           "Java: Supprimer les caractères correspondants à un motif"
simple_title:         "Supprimer les caractères correspondants à un motif"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/java/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Pourquoi

Supprimer des caractères correspondant à un modèle peut être utile pour nettoyer une chaîne de caractères, supprimer des caractères spécifiques tels que les espaces ou pour une validation de saisie.

## Comment faire

```Java
String text = "Bonjour! Comment ça va?";
String pattern = "[!, ¿]";
String result = text.replaceAll(pattern, "");
System.out.println(result);
```
Cet exemple de code utilise la méthode `replaceAll()` pour supprimer tous les `!` et `¿` de la chaîne de caractères `text`, produisant une sortie de `Bonjour Comment ça va`.

## Plongée en profondeur

En utilisant des expressions régulières, une variété de modèles peuvent être utilisés pour supprimer des caractères correspondant à une certaine condition. Par exemple, le modèle `[a-z]` supprimera toutes les lettres minuscules de la chaîne de caractères. En utilisant cette méthode avec prudence, il est possible de nettoyer efficacement et rapidement des chaînes de caractères.

## Voir aussi
- [Documentation officielle de Java sur la méthode replaceAll()](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html#replaceAll-java.lang.String-java.lang.String-)
- [Didacticiel sur les expressions régulières en Java](https://www.vogella.com/tutorials/JavaRegularExpressions/article.html)
- [Code de démonstration des expressions régulières en Java](https://www.geeksforgeeks.org/regular-expression-java/)