---
title:    "Java: Trouver la longueur d'une chaîne de caractères"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/java/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Pourquoi

Trouver la longueur d'une chaîne est une tâche courante dans la programmation Java. Que ce soit pour valider les données entrées par l'utilisateur ou pour manipuler des chaînes de caractères, connaître la longueur d'une chaîne est essentiel dans de nombreux cas.

## Comment Faire

Pour trouver la longueur d'une chaîne en Java, il suffit d'utiliser la méthode `length()` de la classe `String`. Voici un exemple de code qui illustre comment utiliser cette méthode :

```Java
String chaine = "Bonjour";
int longueur = chaine.length();
System.out.println("La longueur de la chaîne est de " + longueur + " caractères.");
```

Lorsque ce code est exécuté, le résultat affiché sera :

```
La longueur de la chaîne est de 7 caractères.
```

Il est également possible d'utiliser la méthode `length()` sur des objets de type `StringBuilder` pour obtenir la longueur de la chaîne contenue dans cet objet.

## Plongée plus Profonde

La méthode `length()` de la classe `String` retourne la longueur de la chaîne en comptant chaque caractère, y compris les espaces et les caractères spéciaux. De plus, cette méthode est sensible à la casse, ce qui signifie que les majuscules et les minuscules seront comptées comme des caractères différents.

Il est également important de noter que la méthode `length()` renvoie un `int`, ce qui signifie qu'elle peut retourner des valeurs allant de 0 à 2^31 - 1. Si la chaîne est trop longue, il est possible qu'elle dépasse cette limite et qu'une erreur se produise.

## Voir Aussi

- [Documentation officielle de la méthode length()](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html#length--)
- [Guide Java : les chaînes de caractères](https://www.jmdoudoux.fr/java/dej/chap-les_chaines_de_caracteres.htm)