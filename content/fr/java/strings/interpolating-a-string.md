---
date: 2024-01-20 17:51:11.019923-07:00
description: "How to: (Comment faire :) En Java, depuis les versions pr\xE9c\xE9dentes,\
  \ on utilisait `String.format`. Avec Java 13, l'interpolation de cha\xEEnes a \xE9\
  t\xE9\u2026"
lastmod: '2024-04-05T21:53:59.127627-06:00'
model: gpt-4-1106-preview
summary: "(Comment faire :) En Java, depuis les versions pr\xE9c\xE9dentes, on utilisait\
  \ `String.format`."
title: "Interpolation de cha\xEEnes de caract\xE8res"
weight: 8
---

## How to:
(Comment faire :)

En Java, depuis les versions précédentes, on utilisait `String.format`. Avec Java 13, l'interpolation de chaînes a été simplifiée grâce au Text Blocks. Voici comment :

```java
// Méthode classique avec String.format
String nom = "Audrey";
String message = String.format("Salut, %s ! Comment ça va ?", nom);
System.out.println(message); // Output: Salut, Audrey ! Comment ça va ?

// Avec l'aperçu de Text Blocks (Java 13 en mode preview)
String nomUtilisateur = "Xavier";
String texte = """
               Bonjour, %s !
               Bienvenue sur notre site.
               """.formatted(nomUtilisateur);
System.out.println(texte);
// Output:
// Bonjour, Xavier !
// Bienvenue sur notre site.
```

## Deep Dive:
(Exploration détaillée :)

Historiquement, l'interpolation de chaînes n'était pas intégrée à Java ; on utilisait `+` ou `StringBuilder`. Ensuite, `String.format` et `MessageFormat` offraient plus de contrôle mais étaient plus verbeux.

Les langages modernes comme Kotlin ou Scala ont intégré l'interpolation, influençant Java. Avec Java 13, les Text Blocks sont une réponse partielle à ce besoin, bien que toujours en développement.

En interne, lorsqu'on interpole avec `String.format`, Java utilise des regex pour remplacer les placeholders. C'est efficace, mais avec Text Blocks, on a une meilleure lisibilité et on peut espérer des optimisations futures au niveau du compilateur.

## See Also:
(Voir aussi :)

- JEP 355: Text Blocks (https://openjdk.java.net/jeps/355) pour une explication détaillée des Text Blocks.
- La documentation officielle de`String.format` (https://docs.oracle.com/en/java/javase/15/docs/api/java.base/java/lang/String.html#format(java.lang.String,java.lang.Object...)) pour tout savoir sur le formatage de chaînes.
- Kotlin documentation sur l'interpolation de chaînes (https://kotlinlang.org/docs/reference/basic-types.html#string-templates) offre un aperçu d'une approche alternative dans un langage JVM voisin.
