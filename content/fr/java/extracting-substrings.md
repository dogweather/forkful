---
title:                "Extraction de sous-chaînes"
html_title:           "Java: Extraction de sous-chaînes"
simple_title:         "Extraction de sous-chaînes"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/java/extracting-substrings.md"
---

{{< edit_this_page >}}

## Pourquoi

Dans certains projets de programmation, il peut être utile d'extraire des sous-chaînes d'une chaîne de caractères plus longue. Cela peut permettre de manipuler plus facilement des données ou de les utiliser dans des algorithmes spécifiques.

## Comment faire

Pour extraire une sous-chaîne en Java, vous pouvez utiliser la méthode substring de la classe String. Cette méthode prend en paramètres l'indice de début et l'indice de fin de votre sous-chaîne et renvoie une nouvelle chaîne contenant les caractères entre ces deux indices.

```Java
//Exemple d'utilisation de la méthode substring
String message = "Je suis un programmeur en Java.";

String sousChaine = message.substring(11, 20);

System.out.println(sousChaine);

//Output : programmeur
```

Si seule l'indice de début est spécifié, la méthode renverra une sous-chaîne à partir de cet indice jusqu'à la fin de la chaîne d'origine. Si l'indice de fin est omis, la méthode renverra une sous-chaîne à partir de l'indice de début jusqu'à la fin de la chaîne d'origine.

Il est également possible d'utiliser des indices négatifs pour commencer à compter à partir de la fin de la chaîne. Par exemple, un indice de -1 correspond au dernier caractère de la chaîne.

```Java
//Exemple d'utilisation d'indices négatifs
String message = "J'adore programmer en Java !";

String sousChaine1 = message.substring(15);

String sousChaine2 = message.substring(2, -8);

System.out.println(sousChaine1);

System.out.println(sousChaine2);

//Output : Java !
//Output : adore programmer
```

## Plongée en profondeur

En Java, les chaînes de caractères sont immuables, ce qui signifie qu'elles ne peuvent pas être modifiées une fois créées. Lorsque vous extrayez une sous-chaîne, une nouvelle chaîne est créée à partir de la chaîne d'origine. Cela peut entraîner un gaspillage de mémoire si vous effectuez de nombreuses extractions de sous-chaînes à partir d'une même chaîne.

Une autre chose à prendre en compte lors de l'extraction de sous-chaînes est la gestion des indices. Faites attention à ne pas dépasser la longueur de la chaîne d'origine et à utiliser des indices valides pour éviter des erreurs d'exécution.

## Voir aussi

- [Documentation officielle de la méthode substring en Java](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html#substring-int-int-)
- [Tutoriel sur les chaînes de caractères en Java](https://www.tutorialspoint.com/java/java_strings.htm)
- [Article sur l'immuabilité en Java](https://www.javacodegeeks.com/2015/09/immutability-in-java.html)