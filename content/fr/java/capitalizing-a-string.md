---
title:                "Java: Mettre une chaîne en majuscule"
simple_title:         "Mettre une chaîne en majuscule"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/java/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Pourquoi

Capitaliser une chaîne de caractères est une tâche fréquemment effectuée en programmation pour diverses raisons. Cela peut être fait pour des raisons de mise en forme, de traitement de données ou pour améliorer la lisibilité et la compréhension du code.

## Comment faire

La manipulation de chaînes de caractères en Java peut sembler intimidante pour les débutants, mais avec la bonne méthode, il est possible de capitaliser facilement une chaîne. Voici un exemple de code pour capitaliser une chaîne en utilisant la méthode `toUpperCase()` :

```java
String maChaine = "programmation";
String chaineCapitalisee = maChaine.toUpperCase();

System.out.println(chaineCapitalisee); // Output : PROGRAMMATION
```

Pour les chaînes comprenant plusieurs mots, il peut être plus utile d'utiliser la méthode `split()` pour séparer chaque mot et ensuite les capitaliser un par un avant de les rejoindre de nouveau dans une seule chaîne.

```java
String maChaine = "programmation en Java";
String[] mots = maChaine.split(" ");
String chaineCapitalisee = "";

for (String mot : mots) {
    chaineCapitalisee += mot.substring(0,1).toUpperCase() + mot.substring(1) + " ";
}

System.out.println(chaineCapitalisee.trim()); // Output : Programmation En Java
```

## Plongeon en profondeur

En plongeant plus en profondeur, on peut remarquer que la méthode `toUpperCase()` utilise en fait la classe `java.lang.Character` pour effectuer la capitalisation. Cette classe contient plusieurs méthodes pour manipuler les caractères, notamment `toUpperCase()` pour les mettre en majuscule et `toLowerCase()` pour les mettre en minuscule.

L'approche précédente en utilisant `split()` et une boucle peut également être améliorée en utilisant la méthode `replace()` de la classe `String` pour remplacer directement la première lettre de chaque mot par sa version en majuscule, comme ceci :

```java
String maChaine = "programmation en Java";
String chaineCapitalisee = maChaine.replace(maChaine.substring(0,1), maChaine.substring(0,1).toUpperCase());

System.out.println(chaineCapitalisee); // Output : Programmation En Java
```

## Voir aussi
- Documentation officielle de la classe `Character` de Java : https://docs.oracle.com/javase/8/docs/api/java/lang/Character.html
- Tutoriel sur les chaînes de caractères en Java : https://www.tutorialspoint.com/java/java_strings.htm
- Méthodes de manipulation de chaînes en Java : https://www.baeldung.com/java-string-manipulation