---
title:                "Java: Convertir une chaîne en minuscules"
simple_title:         "Convertir une chaîne en minuscules"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/java/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Pourquoi

La conversion d'une chaîne de caractères en minuscules peut être utile pour de nombreuses raisons en programmation, notamment pour faciliter la comparaison de chaînes, pour l'analyse de données ou pour afficher du texte en minuscules dans l'interface utilisateur.

## Comment Faire

Pour convertir une chaîne en minuscules en utilisant Java, vous pouvez utiliser la méthode `toLowerCase()` de la classe `String`. Voici un exemple de code et son résultat :

```Java
String str = "Bonjour Tout Le Monde!";
System.out.println(str.toLowerCase());
// Output : bonjour tout le monde!
```

Vous pouvez également utiliser la classe `Character` pour convertir des caractères individuels en minuscules. Voici un exemple :

```Java
char c = 'A';
System.out.println(Character.toLowerCase(c));
// Output : a
```

## Plongée Profonde

En Java, les chaînes de caractères sont immuables, ce qui signifie qu'elles ne peuvent pas être modifiées une fois créées. Ainsi, lors de la conversion d'une chaîne en minuscules, une nouvelle chaîne est créée et renvoyée par la méthode `toLowerCase()`, avec toutes les lettres en minuscules.

Il est également important de noter que la conversion en minuscules dépend de la locale de l'ordinateur, car certaines langues n'ont pas de notion de majuscules et de minuscules. Vous pouvez spécifier une locale lors de la conversion en utilisant une surcharge de la méthode `toLowerCase()`, par exemple :

```Java
String str = "HÉLLO";
System.out.println(str.toLowerCase(Locale.FRENCH));
// Output : héllö
```

## Voir Aussi

- [Documentation officielle de la méthode toLowerCase()](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html#toLowerCase--)
- [Tutoriel sur les chaînes de caractères en Java](https://www.w3schools.com/java/java_strings.asp)
- [Article sur les locales en Java](https://www.tutorialspoint.com/java/java_locale.htm)