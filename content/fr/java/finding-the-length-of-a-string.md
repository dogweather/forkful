---
title:                "Java: Trouver la longueur d'une chaîne de caractères"
simple_title:         "Trouver la longueur d'une chaîne de caractères"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/java/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Pourquoi

Trouver la longueur d'une chaîne de caractères est une tâche courante en programmation. Cela peut être utile pour effectuer des manipulations et des vérifications sur une chaîne de caractères spécifique, comme vérifier si elle dépasse une certaine longueur ou extraire une portion spécifique de la chaîne.

## Comment faire

Pour trouver la longueur d'une chaîne en Java, il suffit d'utiliser la méthode "length()" de la classe String. Voici un exemple de code pour illustrer cela :

```Java
String str = "Bonjour";
int length = str.length();
System.out.println(length); // Output : 7
```

La méthode "length()" renvoie un entier qui correspond à la longueur de la chaîne. Dans l'exemple ci-dessus, la chaîne "Bonjour" a une longueur de 7 caractères, donc la valeur de la variable "length" sera de 7.

Il est important de noter que cette méthode renvoie la longueur de la chaîne en termes de caractères, et non en termes de mots ou d'espaces. Par exemple, si la chaîne contient des accents ou des caractères spéciaux, ceux-ci seront également comptabilisés dans la longueur.

## Plongée en profondeur

La méthode "length()" s'appuie sur la propriété "length" de la classe String, qui stocke la longueur de la chaîne en interne. Cela signifie que l'appel de la méthode "length()" n'effectue pas réellement un parcours de la chaîne pour trouver sa longueur, ce qui le rend très efficace et rapide.

Il est également important de noter que la longueur d'une chaîne ne peut pas être modifiée, car les objets String sont immuables en Java. Cela signifie qu'une fois qu'une chaîne est définie, sa longueur ne peut plus être modifiée.

## Voir aussi

- Documentation de la méthode String.length() : https://docs.oracle.com/javase/8/docs/api/java/lang/String.html#length()
- Exemples d'utilisation de la méthode String.length() : https://www.geeksforgeeks.org/string-length-method-in-java-with-examples/