---
title:                "Convertir une chaîne en minuscules"
html_title:           "Java: Convertir une chaîne en minuscules"
simple_title:         "Convertir une chaîne en minuscules"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/java/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Pourquoi
Les développeurs utilisent souvent la conversion de chaîne en minuscules pour faciliter la manipulation de données et améliorer les performances du code. Par exemple, il peut être utile de comparer des chaînes de caractères sans tenir compte de la casse, ou de trier une liste de mots dans l'ordre alphabétique sans se soucier des majuscules.

## Comment faire
La conversion d'une chaîne en minuscules en Java est assez simple. Tout d'abord, il faut utiliser la méthode `toLowerCase()` de la classe `String` sur la chaîne à convertir. Par exemple :
```java
String phrase = "Bonjour le MONDE !";
String phraseEnMinuscules = phrase.toLowerCase();

System.out.println(phraseEnMinuscules); // affiche "bonjour le monde !"
```
Comme on peut le voir dans l'exemple ci-dessus, la méthode `toLowerCase()` renvoie une nouvelle chaîne de caractères avec tous les caractères en minuscules.

Il est également possible de passer un paramètre `Locale` à la méthode, pour prendre en compte les spécificités de la langue. Par exemple, pour la langue française, on peut utiliser `Locale.FRENCH` pour gérer les accents et les caractères spéciaux. Voici un exemple :
```java
String phrase = "Élémentaire, mon cher Watson !";
String phraseEnMinuscules = phrase.toLowerCase(Locale.FRENCH);

System.out.println(phraseEnMinuscules); // affiche "élémentaire, mon cher watson !"
```

## Plongée en profondeur
Sous le capot, la méthode `toLowerCase()` utilise la table de correspondance des caractères ASCII pour convertir les caractères en minuscules. Cependant, cette méthode n'est pas adaptée pour toutes les langues, car certaines ont des règles de conversion plus complexes. Par exemple, en turc, la conversion de la lettre "I" en minuscule donne un caractère différent ("ı") alors que la table ASCII donne "i". Dans ces cas-là, il est préférable d'utiliser la méthode `toUpperCase()` avec le paramètre `Locale` adapté.

Il est également important de noter que la méthode `toLowerCase()` renvoie toujours une nouvelle chaîne de caractères, même si la chaîne originale était déjà en minuscules. Ainsi, si vous utilisez cette méthode plusieurs fois dans un même code, cela peut entraîner une perte de performance. Dans ce cas, il peut être judicieux de d'abord vérifier si la chaîne est déjà en minuscules avant d'appeler la méthode.

## Voir aussi
- [Java String toLowerCase() method documentation](https://docs.oracle.com/javase/10/docs/api/java/lang/String.html#toLowerCase())
- [Java Locale class documentation](https://docs.oracle.com/javase/10/docs/api/java/util/Locale.html)