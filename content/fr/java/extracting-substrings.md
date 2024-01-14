---
title:                "Java: Extraction de sous-chaînes"
simple_title:         "Extraction de sous-chaînes"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/java/extracting-substrings.md"
---

{{< edit_this_page >}}

## Pourquoi

La manipulation de chaînes de caractères est une partie essentielle de la programmation en Java, et une tâche fréquemment nécessaire est l'extraction de sous-chaînes. L'extraction de sous-chaînes peut être utile pour obtenir des informations spécifiques à partir d'une chaîne de caractères plus grande, comme un nom de fichier ou une adresse e-mail. Dans cet article, nous allons explorer comment extraire des sous-chaînes en Java.

## Comment faire

L'extraction de sous-chaînes en Java peut être réalisée en utilisant la méthode `substring()` de la classe String. Cette méthode prend deux paramètres : l'index de début et l'index de fin de la sous-chaîne que vous souhaitez extraire. Voici un exemple de code :

```Java
String chaine = "Bonjour tout le monde !";
String sousChaine = chaine.substring(8, 16);
System.out.println(sousChaine); // affichera "tout le m"
```

Dans cet exemple, nous avons extrait une sous-chaîne de la chaîne originale en spécifiant les indices de début et de fin. Il est important de noter que l'indice de fin n'est pas inclus dans la sous-chaîne, c'est pourquoi nous avons obtenu "tout le m" et non pas "tout le monde".

Vous pouvez également utiliser la méthode `length()` pour obtenir la longueur de la chaîne et ainsi définir l'indice de fin comme `chaine.length()` si vous souhaitez extraire la sous-chaîne jusqu'à la fin de la chaîne.

Il est également possible d'utiliser un seul paramètre dans la méthode `substring()`, qui indiquera l'indice de début jusqu'à la fin de la chaîne. Par exemple, `chaine.substring(5)` extraira tous les caractères à partir de l'indice 5 jusqu'à la fin de la chaîne.

## Plongée en profondeur

La méthode `substring()` utilise des indices 0-basés, ce qui signifie que le premier caractère de la chaîne a un indice de 0 et non pas de 1. De plus, en utilisant un seul paramètre comme dans l'exemple précédent, l'indice par défaut de fin est la longueur de la chaîne. Cela peut être utile pour extraire la dernière partie d'une chaîne, comme le domaine d'une adresse e-mail.

Il est également possible de combiner l'extraction de sous-chaînes avec d'autres méthodes de la classe String, comme `indexOf()`, pour obtenir des résultats plus précis et flexibles.

N'oubliez pas que les chaînes en Java sont immuables, cela signifie que la méthode `substring()` ne modifiera pas la chaîne originale mais renverra une nouvelle chaîne. Si vous souhaitez modifier une chaîne, vous devrez utiliser des méthodes telles que `replace()`.

## Voir aussi

- [Documentation officielle Oracle sur la classe String](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html#substring-int-)
- [Tutoriel sur les chaînes en Java](https://www.tutorialspoint.com/java/java_strings.htm)
- [Vidéo sur l'utilisation de la méthode `substring()`](https://www.youtube.com/watch?v=F27KkJAaCgA)