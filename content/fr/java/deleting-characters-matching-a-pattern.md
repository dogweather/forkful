---
title:                "Java: Suppression des caractères correspondant à un modèle"
programming_language: "Java"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/java/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Pourquoi

Vous êtes-vous déjà retrouvé dans une situation où vous vouliez supprimer des caractères spécifiques dans une chaîne de texte en Java ? Peut-être que vous aviez une chaîne de caractères très longue et que vous vouliez vous débarrasser de tous les espaces, ou peut-être que vous vouliez supprimer tous les nombres d'une chaîne contenant des lettres et des chiffres mélangés. Dans ces cas-là, supprimer des caractères correspondant à un modèle peut être une solution pratique et efficace. Dans cet article, nous allons vous montrer comment le faire en utilisant Java.

## Comment faire

Supprimer des caractères correspondant à un modèle en Java peut être fait de différentes manières, mais nous allons vous montrer une méthode simple et efficace en utilisant la méthode `replaceAll()` de la classe `String`.

```java
String texte = "Beaucoup d'espaces dans ce texte.";
// Supprime tous les espaces dans le texte
String nouveauTexte = texte.replaceAll(" ", "");
System.out.println(nouveauTexte);
```

Dans cet exemple, nous avons utilisé `replaceAll()` pour remplacer tous les espaces dans la chaîne `texte` par une chaîne vide, ce qui donne une nouvelle chaîne sans aucun espace. Voici la sortie de ce code :

```
Beaucoupd'espacesdanscetexte.
```

Nous pouvons également utiliser des modèles plus complexes en utilisant des expressions régulières. Par exemple, si nous voulons supprimer tous les nombres dans une chaîne de texte, nous pouvons utiliser l'expression régulière `\\d` qui correspond à tous les chiffres.

```java
String texte = "J'ai 27 ans.";
// Supprime tous les chiffres dans le texte
String nouveauTexte = texte.replaceAll("\\d", "");
System.out.println(nouveauTexte);
```

La sortie de ce code sera :

```
J'ai ans.
```

## Plongée en profondeur

La méthode `replaceAll()` utilise des expressions régulières pour trouver les correspondances à supprimer dans la chaîne de texte. Il est important de bien comprendre ces expressions régulières pour pouvoir les utiliser correctement. Par exemple, la lettre `d` dans l'expression `\\d` correspond à tous les chiffres, mais si nous écrivons simplement `d`, cela signifiera n'importe quelle lettre de l'alphabet, ce qui peut donner des résultats inattendus.

De plus, la méthode `replaceAll()` remplace toutes les correspondances trouvées dans la chaîne, mais si vous voulez supprimer une seule occurrence, vous devrez utiliser la méthode `replace()` au lieu de `replaceAll()`.

## Voir également

- [Tutoriel sur les expressions régulières en Java](https://www.tutorialspoint.com/java/java_regular_expressions.htm)
- [Documentation officielle de la classe String en Java](https://docs.oracle.com/javase/7/docs/api/java/lang/String.html)