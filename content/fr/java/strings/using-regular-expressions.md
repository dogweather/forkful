---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:17:13.160140-07:00
description: "Les expressions r\xE9guli\xE8res (regex) en Java vous permettent de\
  \ d\xE9finir des motifs sp\xE9cifiques pour rechercher, manipuler ou valider des\
  \ cha\xEEnes de\u2026"
lastmod: 2024-02-19 22:05:16.390420
model: gpt-4-0125-preview
summary: "Les expressions r\xE9guli\xE8res (regex) en Java vous permettent de d\xE9\
  finir des motifs sp\xE9cifiques pour rechercher, manipuler ou valider des cha\xEE\
  nes de\u2026"
title: "Utilisation des expressions r\xE9guli\xE8res"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?

Les expressions régulières (regex) en Java vous permettent de définir des motifs spécifiques pour rechercher, manipuler ou valider des chaînes de caractères dans votre code. Les programmateurs les utilisent pour des tâches telles que l'analyse de fichiers journaux, la validation des entrées utilisateur, ou la recherche de motifs spécifiques dans un texte, permettant un traitement sophistiqué des chaînes avec un effort minimal.

## Comment :

Le support intégré de Java pour les regex se fait principalement à travers les classes `Pattern` et `Matcher` dans le package `java.util.regex`. Voici un exemple simple pour trouver et imprimer toutes les occurrences d'un mot dans une chaîne, sans tenir compte de la casse :

```java
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class RegexExample {
    public static void main(String[] args) {
        String text = "Regex is great for parsing. Parsing with regex is powerful.";
        String wordToFind = "parsing";
        
        Pattern pattern = Pattern.compile(wordToFind, Pattern.CASE_INSENSITIVE);
        Matcher matcher = pattern.matcher(text);
        
        while (matcher.find()) {
            System.out.println("Trouvé '" + matcher.group() + "' à la position " + matcher.start());
        }
    }
}
```

Sortie :
```
Trouvé 'parsing' à la position 16
Trouvé 'Parsing' à la position 31
```

Pour des tâches comme diviser des chaînes, vous pouvez utiliser la méthode `split()` de la classe `String` avec un regex :

```java
public class SplitExample {
    public static void main(String[] args) {
        String text = "Java,Python,Ruby,JavaScript";
        String[] languages = text.split(",");
        
        for (String language : languages) {
            System.out.println(language);
        }
    }
}
```

Sortie :
```
Java
Python
Ruby
JavaScript
```

Lorsque vous travaillez avec des regex en Java, il peut arriver que l'utilisation d'une bibliothèque externe puisse simplifier des tâches complexes. L'une des bibliothèques tierces populaires pour travailler avec les regex en Java est `Apache Commons Lang`. Elle offre des utilitaires comme `StringUtils` qui rendent certaines tâches regex plus simples. Voici comment l'utiliser pour compter les occurrences d'une sous-chaîne :

```java
import org.apache.commons.lang3.StringUtils;

public class CommonsLangExample {
    public static void main(String[] args) {
        String text = "Regex makes text processing easier. Processing text with regex is efficient.";
        String substring = "processing";
        
        int count = StringUtils.countMatches(text, substring);
        System.out.println("'" + substring + "' apparaît " + count + " fois.");
    }
}
```

Pour utiliser Apache Commons Lang, vous devez l'inclure dans votre projet. Si vous utilisez Maven, ajoutez cette dépendance à votre `pom.xml` :

```xml
<dependency>
    <groupId>org.apache.commons</groupId>
    <artifactId>commons-lang3</artifactId>
    <version>3.12.0</version> <!-- Vérifiez pour la dernière version -->
</dependency>
```

Sortie :
```
'processing' apparaît 2 fois.
```
