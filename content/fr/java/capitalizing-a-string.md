---
title:    "Java: Mettre une chaîne en majuscule"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/java/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Pourquoi

Vous êtes-vous déjà demandé pourquoi il est important de capitaliser une chaîne de caractères dans votre code Java ? Il y a différentes raisons pour lesquelles vous pourriez vouloir le faire. Par exemple, cela pourrait rendre votre code plus lisible pour vous et pour d'autres développeurs ou cela pourrait être une exigence de la syntaxe du langage. Quelle que soit la raison, capitaliser une chaîne est une compétence utile à avoir dans votre boîte à outils de programmation.

## Comment faire

Capitaliser une chaîne de caractères en Java est un processus simple et rapide. Vous pouvez utiliser la méthode "toUpperCase()" pour convertir la chaîne en lettres majuscules. Voici un exemple de code :

```Java
String str = "exemple";
String strCapitalized = str.toUpperCase();
System.out.println(strCapitalized);
```

Ce code va prendre la chaîne "exemple" et la convertir en "EXEMPLE", puis l'imprimer en sortie. N'hésitez pas à expérimenter avec différentes chaînes et à voir comment elles sont converties en majuscules.

## Approfondissement

Maintenant que vous savez comment capitaliser une chaîne en Java, vous pouvez vous demander ce qui se passe en coulisse lorsque vous utilisez la méthode "toUpperCase()". En fait, cette méthode utilise les règles de l'alphabet anglais pour convertir les lettres minuscules en majuscules. Cela signifie qu'elle ne fonctionne pas nécessairement pour toutes les langues et que vous devrez peut-être rechercher une méthode différente pour capitaliser des chaînes dans d'autres langues.

De plus, il existe d'autres moyens de capitaliser une chaîne en Java, tels que l'utilisation de boucles et de conditions pour examiner chaque caractère de la chaîne et le convertir en majuscule. Cela pourrait être un bon exercice pour approfondir vos connaissances en programmation Java.

## Voir aussi

Maintenant que vous savez comment capitaliser une chaîne en Java, vous pourriez également être intéressé par d'autres compétences de manipulation de chaînes de caractères, telles que la concaténation de chaînes ou la recherche de sous-chaînes. Voici quelques liens utiles pour vous aider à approfondir vos connaissances :

- [Various ways to capitalize a string in Java](https://www.baeldung.com/java-uppercase-string)
- [String manipulation in Java](https://www.tutorialspoint.com/java/java_string_split.htm)
- [Java documentation for String class](https://docs.oracle.com/javase/7/docs/api/java/lang/String.html)