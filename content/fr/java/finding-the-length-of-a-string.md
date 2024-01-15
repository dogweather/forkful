---
title:                "Trouver la longueur d'une chaîne de caractères"
html_title:           "Java: Trouver la longueur d'une chaîne de caractères"
simple_title:         "Trouver la longueur d'une chaîne de caractères"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/java/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Pourquoi

Vous vous demandez peut-être pourquoi il est important de trouver la longueur d'une chaîne en Java. Eh bien, cela peut être utile dans de nombreuses situations de programmation où vous avez besoin de connaître le nombre exact de caractères dans une chaîne, comme dans la validation de mots de passe ou la manipulation de données.

## Comment faire

Pour trouver la longueur d'une chaîne en Java, vous pouvez utiliser la méthode `length()` de la classe `String`. Voici un exemple de code :

```Java
String str = "Bonjour tout le monde!";
int length = str.length();
System.out.println("La longueur de la chaîne est : " + length);
```

La sortie de ce code sera : `La longueur de la chaîne est : 22`.

Vous pouvez également utiliser une boucle pour parcourir chaque caractère de la chaîne et incrémenter une variable de compteur pour obtenir la longueur. Voici un exemple de code avec une boucle `for` :

```Java
String str = "Hello";
int count = 0;

for(int i = 0; i < str.length(); i++) {
    count++;
}

System.out.println("La longueur de la chaîne est : " + count);
```

La sortie de ce code sera également : `La longueur de la chaîne est : 5`.

## Approfondissement

Il est important de noter que la méthode `length()` compte tous les caractères de la chaîne, y compris les espaces. Par exemple, si vous calculez la longueur de "Bonjour tout le monde !", vous obtiendrez 22, car il y a 22 caractères, y compris l'espace avant le point d'exclamation.

De plus, la méthode `length()` renvoie un `int`, ce qui signifie que vous ne pourrez pas trouver la longueur de chaînes très longues, car le type `int` ne peut stocker que jusqu'à 2 147 483 647. Si vous devez calculer la longueur de chaînes plus grandes, vous devrez utiliser une autre méthode ou une autre approche de programmation.

Enfin, veillez à ne pas confondre la méthode `length()` avec la méthode `size()` qui mesure la taille d'une collection, telle qu'une liste ou un tableau, en comptant le nombre d'éléments qu'elle contient.

## Voir aussi

- Tutoriel sur les chaînes en Java : https://www.w3schools.com/java/java_strings.asp
- Documentation officielle sur la méthode `length()` : https://docs.oracle.com/javase/7/docs/api/java/lang/String.html#length()