---
title:                "Java: Majuscule d'une chaîne de caractères"
programming_language: "Java"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/java/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Pourquoi

Dans la programmation Java, il est souvent nécessaire de manipuler des chaînes de caractères. L'une des manipulations courantes est la mise en majuscule d'une chaîne de caractères. Dans cet article, nous allons explorer pourquoi il est important de maîtriser cette technique et comment le faire de manière efficace.

## Comment faire

La mise en majuscule d'une chaîne de caractères en Java est une opération simple grâce à la méthode `toUpperCase ()`. Cette méthode convertit tous les caractères d'une chaîne en majuscules et renvoie le résultat. Voyons un exemple concret de son utilisation :

```Java
String myString = "bonjour";
String upperCaseString = myString.toUpperCase();
System.out.println(upperCaseString);
```

La sortie de ce code sera `"BONJOUR"`, avec tous les caractères en majuscules. Il est également possible de ne convertir qu'une partie d'une chaîne en majuscules en utilisant la méthode `substring ()` avant `toUpperCase ()`.

## Plongeons plus profondément

Il peut sembler simple de mettre en majuscule une chaîne de caractères, mais il est important de comprendre comment cela fonctionne en interne afin de pouvoir l'utiliser de manière efficace. Lorsque nous créons une nouvelle chaîne de caractères en utilisant `toUpperCase ()`, une nouvelle instance de chaîne de caractères est créée en mémoire. Cela peut entraîner une surcharge de la mémoire si nous mettons en majuscule de grandes chaînes ou si nous les utilisons fréquemment dans notre code. Pour éviter cela, il est préférable de travailler avec des objets `StringBuilder` et de manipuler directement les caractères de la chaîne. De plus, il est important de garder à l'esprit que la méthode `toUpperCase ()` utilise la locale de la machine sur laquelle le code est exécuté, ce qui peut entraîner des résultats différents selon la localisation.

## Voir aussi

Pour en savoir plus sur la manipulation des chaînes de caractères en Java, vous pouvez consulter ces ressources :

- [Documentation officielle Java sur les chaînes de caractères](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html)
- [Tutoriel sur la manipulation des chaînes en Java](https://www.baeldung.com/java-string-manipulation)