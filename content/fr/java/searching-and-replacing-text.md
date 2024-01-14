---
title:    "Java: Recherche et remplacement de texte"
keywords: ["Java"]
---

{{< edit_this_page >}}

## Pourquoi

La recherche et le remplacement de texte sont des tâches courantes dans la programmation Java. Cela permet de modifier rapidement des chaînes de caractères pour répondre à des besoins spécifiques. Dans cet article, nous allons explorer comment réaliser cette opération de manière efficace et rapide.

## Comment faire

La méthode la plus simple pour rechercher et remplacer du texte est d'utiliser la fonction `replace()` de la classe `String`. Cette fonction prend en paramètre la chaîne de caractères à rechercher et la chaîne de remplacement et retourne une nouvelle chaîne avec les modifications apportées.

```
// Exemple avec la chaîne "bonjour" et sa version remplacée "hello"
String texte = "Bonjour tout le monde!";
texte = texte.replace("bonjour", "hello");
System.out.println(texte);
```

La sortie de ce code serait "Hello tout le monde!" car la chaîne "bonjour" a été remplacée par "hello". Il est important de noter que la fonction `replace()` ne modifie pas la chaîne d'origine, elle retourne un nouvel objet `String` avec les modifications.

La fonction `replaceAll()` est également disponible pour remplacer toutes les occurrences d'une chaîne de caractères dans une autre. Elle utilise des expressions régulières pour identifier les parties à remplacer.

```
String texte = "Le chat noir dort sous la table.";
texte = texte.replaceAll("noir", "blanc");
System.out.println(texte);
```

La sortie de ce code serait "Le chat blanc dort sous la table." car la chaîne "noir" a été remplacée par "blanc".

## Plongée en profondeur

Il est également possible d'utiliser des expressions régulières avec la fonction `replace()` en utilisant la méthode `replaceFirst()`. Cela permet de remplacer seulement la première occurrence d'une chaîne dans une autre en utilisant des expressions régulières.

```
String texte = "5 pommes, 3 oranges et 2 bananes.";
texte = texte.replaceFirst("\\d+", "un");
System.out.println(texte);
```

La sortie de ce code serait "Un pommes, 3 oranges et 2 bananes." car seule la première occurrence d'un nombre a été remplacée par "un".

Il est également possible d'utiliser des boucles et des structures de contrôle pour réaliser des remplacements plus complexes. Par exemple, si nous voulons remplacer tous les nombres par leur double, nous pourrions utiliser la fonction `replace()` à l'intérieur d'une boucle et utiliser la fonction `Integer.parseInt()` pour convertir les chaînes en entiers et effectuer l'opération de multiplication.

## Voir aussi

- [Documentation de la classe String](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html)
- [Tutoriel sur les expressions régulières en Java](https://www.vogella.com/tutorials/JavaRegularExpressions/article.html)