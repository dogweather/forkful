---
title:    "Java: Majuscule d'une chaîne de caractères."
keywords: ["Java"]
---

{{< edit_this_page >}}

## Pourquoi

Il est courant dans la programmation de devoir manipuler des chaînes de caractères. L'une des manipulations les plus courantes est de mettre en majuscule la première lettre de chaque mot dans une phrase ou un nom. Cela peut être utile pour de nombreuses raisons, comme l'affichage de noms propres ou l'alignement de données. Dans cet article, nous allons explorer comment capitaliser une chaîne de caractères en utilisant Java.

## Comment faire 

Pour capitaliser une chaîne de caractères, nous pouvons utiliser la méthode `capitalize()` de la classe `StringUtils` de la bibliothèque Apache Commons. Par exemple, si nous avons une chaîne de caractères "bonjour tout le monde", nous pouvons l'utiliser de la manière suivante :

```Java
StringUtils.capitalize("bonjour tout le monde");
```

Cela renverra la chaîne de caractères "Bonjour tout le monde".

Une autre option consiste à utiliser la méthode `toUpperCase()` avec `substring()` de la classe `String`. Dans cet exemple, nous devons d'abord extraire la première lettre de la chaîne de caractères, la mettre en majuscule, puis la concaténer avec le reste de la chaîne de caractères en la mettant en minuscule. Voici comment cela pourrait être codé :

```Java
String str = "bonjour tout le monde";
str = str.substring(0, 1).toUpperCase() + str.substring(1).toLowerCase();
System.out.println(str);
```

Cela produira le même résultat que la méthode `capitalize()`.

## Plongée en profondeur

Il est important de noter que la méthode `capitalize()` ne mettra en majuscule que la première lettre de la chaîne de caractères, tandis que la deuxième méthode mettra en majuscule la première lettre de chaque mot. De plus, si la chaîne de caractères contient des caractères spéciaux comme des accents, ils ne seront pas gérés correctement. Dans ce cas, il peut être utile d'utiliser la méthode `toUpperCase()` en combinaison avec `Locale` pour spécifier la langue de la chaîne de caractères.

## Voir aussi

- [Documentation de la méthode `capitalize()` dans la classe `StringUtils`](https://commons.apache.org/proper/commons-lang/apidocs/org/apache/commons/lang3/StringUtils.html#capitalize-java.lang.String-)
- [Documentation de la méthode `substring()` dans la classe `String`](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html#substring-int-int-)
- [Documentation de la classe `Locale`](https://docs.oracle.com/javase/8/docs/api/java/util/Locale.html)