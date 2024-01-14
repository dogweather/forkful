---
title:                "Java: Trouver la longueur d'une chaîne de caractères."
programming_language: "Java"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/java/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

# Pourquoi

L'une des tâches les plus courantes en programmation est de manipuler des chaînes de caractères, et l'un des défis les plus courants lors de cette manipulation est de connaître la longueur d'une chaîne. Cela peut sembler simple à première vue, mais il y a plus d'une façon de trouver la longueur d'une chaîne en Java. Dans cet article, nous allons explorer différentes méthodes pour y parvenir.

## Comment faire

Il existe différentes méthodes pour trouver la longueur d'une chaîne en Java. La plus simple consiste à utiliser la méthode `length()` de la classe String, qui renvoie le nombre de caractères dans la chaîne. Voici un exemple de code qui utilise cette méthode :

```java
String str = "Bonjour";
System.out.println("La longueur de la chaîne est : " + str.length());
// Output: La longueur de la chaîne est : 7
```

Une autre façon de trouver la longueur d'une chaîne est d'utiliser la méthode `toCharArray()` de la classe String, qui renvoie un tableau de caractères contenant tous les caractères de la chaîne. Ensuite, il suffit de compter le nombre d'éléments dans ce tableau pour obtenir la longueur de la chaîne. Voici un exemple de code utilisant cette méthode :

```java
String str = "Bonjour";
char[] charArray = str.toCharArray();
System.out.println("La longueur de la chaîne est : " + charArray.length);
// Output: La longueur de la chaîne est : 7
```

Il est également possible d'utiliser une boucle pour parcourir la chaîne caractère par caractère et compter le nombre de caractères. Voici un exemple de code utilisant cette méthode :

```java
String str = "Bonjour";
int count = 0;
for(int i = 0; i < str.length(); i++){
    count++;
}
System.out.println("La longueur de la chaîne est : " + count);
// Output: La longueur de la chaîne est : 7
```

## Plongée en profondeur

Lorsqu'on utilise la méthode `length()` pour trouver la longueur d'une chaîne, il est important de noter que cette méthode renvoie le nombre de caractères, et non le nombre d'octets utilisés pour stocker la chaîne en mémoire. En effet, en Java, les caractères sont stockés sur 16 bits, et il faudra donc deux octets pour stocker un caractère au lieu d'un seul octet en mémoire. Cela peut causer des problèmes lorsqu'on travaille avec des chaînes contenant des caractères multibyte, tels que les caractères accentués ou les emoji.

## Voir aussi

- [Documentation officielle Oracle sur la classe String](https://docs.oracle.com/javase/10/docs/api/java/lang/String.html)
- [Article sur la méthode `length()` de la classe String](https://docs.oracle.com/javase/tutorial/java/data/strings.html)
- [Article sur la méthode `toCharArray()` de la classe String](https://docs.oracle.com/javase/10/docs/api/java/lang/String.html#toCharArray())