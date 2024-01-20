---
title:                "Suppression de caractères correspondant à un motif"
html_title:           "C: Suppression de caractères correspondant à un motif"
simple_title:         "Suppression de caractères correspondant à un motif"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/java/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

# Éliminer les caractères correspondants à un motif en Java

## Qu'est-ce et Pourquoi?
En programmation, supprimer des caractères qui correspondent à un motif signifie chercher des séquences spécifiques de caractères dans une chaîne et les retirer. Cette action aide à nettoyer ou à formater nos données.

## Comment faire:
Par exemple, éliminer tous les espaces d'une chaîne en Java. Utilisons la méthode `replaceAll()` avec `\\s` qui correspond à un espace.

```Java
public class MainClass {
    public static void main(String[] args) {
        String text = "Bonjour, comment ca va ?";
        String result = text.replaceAll("\\s", "");
        System.out.println(result); // Affiche: Bonjour,commentcava?
    }
}
```

## Exploration en Profondeur
Historiquement, `replaceAll()` est utilisée depuis Java 1.4; elle offre une capacité robuste d'élimination de motifs. Néanmoins, d'autres méthodes peuvent être utilisées, comme `Pattern` et `Matcher`.

L'implémentation du `replaceAll()` se fait via une compilation du motif à un objet `Pattern`, qui est ensuite utilisé pour créer un objet `Matcher`. Cette information est utile pour comprendre le coût potentiel en termes de performance.

```Java
Pattern SpacesPattern = Pattern.compile("\\s");
Matcher matcher = SpacesPattern.matcher(text);
String result = matcher.replaceAll("");
```

## Voir Aussi
- Tutorial de Java Regex: https://docs.oracle.com/javase/tutorial/essential/regex/
- La méthode replaceAll(): https://docs.oracle.com/en/java/javase/15/docs/api/java.base/java/lang/String.html#replaceAll(java.lang.String,java.lang.String)
- API Pattern: https://docs.oracle.com/en/java/javase/15/docs/api/java.base/java/util/regex/Pattern.html
- API Matcher: https://docs.oracle.com/en/java/javase/15/docs/api/java.base/java/util/regex/Matcher.html