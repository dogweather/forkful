---
title:    "Java: Concaténation de chaînes de caractères"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/java/concatenating-strings.md"
---

{{< edit_this_page >}}

## Pourquoi

Concaténer des chaînes de caractères est une tâche courante en programmation Java. Cela permet de joindre deux ou plusieurs chaînes pour en former une seule. Cela peut être utile pour créer des messages personnalisés, des requêtes SQL ou même pour afficher du texte à l'écran.

## Comment faire

Pour concaténer des chaînes de caractères en Java, il existe plusieurs méthodes. L'une des plus courantes est d'utiliser l'opérateur de concaténation (+). Voici un exemple de code :

```Java
String str1 = "Bonjour";
String str2 = "le monde!";
System.out.println(str1 + " " + str2);
```

Cela produira l'output suivant : "Bonjour le monde!"

Une autre méthode courante est d'utiliser la méthode `concat()` de la classe `String`. Voici un exemple de code :

```Java
String str1 = "Bonjour";
String str2 = "le monde!";
System.out.println(str1.concat(" ").concat(str2));
```

Cela produira également l'output : "Bonjour le monde!"

## Décortiquons le sujet

Il est important de comprendre que les chaînes de caractères sont immutables en Java, ce qui signifie qu'elles ne peuvent pas être modifiées une fois créées. Lorsque nous concaténons des chaînes, nous créons en fait une nouvelle chaîne contenant les valeurs des chaînes d'origine. Ceci est important à savoir car cela peut entraîner une utilisation excessive de la mémoire si de nombreuses chaînes sont concaténées.

De plus, il est important de noter que les objets `String` en Java sont stockés dans une zone de la mémoire appelée la "String pool". Cela signifie que si deux chaînes sont identiques, elles pointeront vers la même référence dans la String pool, ce qui peut avoir un impact sur les performances si de nombreuses opérations de concaténation sont effectuées.

## Voir aussi

- [Documentation officielle Java sur les chaînes](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html)
- [Article sur l'immuabilité en Java](https://dzone.com/articles/strings-are-constants)
- [Article sur l'utilisation de la String pool en Java](https://www.baeldung.com/java-string-pool)