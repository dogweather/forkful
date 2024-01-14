---
title:                "Java: Concaténer des chaînes de caractères"
programming_language: "Java"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/java/concatenating-strings.md"
---

{{< edit_this_page >}}

## Pourquoi

La concaténation de chaînes de caractères est un élément essentiel de la programmation en Java. Elle permet de combiner plusieurs chaînes de caractères pour former une seule et unique chaîne. Cela peut être utile dans de nombreuses situations, notamment pour créer des messages d'erreur personnalisés, des URL dynamiques ou tout simplement pour afficher du texte dans la console.

## Comment faire

En Java, la concaténation de chaînes de caractères peut se faire de différentes manières. Voici quelques exemples de code avec leur résultat :

```Java
// Utilisation de l'opérateur "+"
String nom = "Jean";
String message = "Bonjour " + nom;
// Résultat : Bonjour Jean

// Utilisation de la méthode concat()
String nom = "Paul";
String message = "Bienvenue ".concat(nom);
// Résultat : Bienvenue Paul

// Concaténation à l'aide d'un StringBuilder
StringBuilder sb = new StringBuilder();
sb.append("Hello").append(" ").append("World");
// Résultat : Hello World
```

Il est également possible de concaténer des variables de différents types en les convertissant en chaîne de caractères à l'aide de la méthode `String.valueOf()`. Par exemple :

```Java
int age = 25;
String message = "J'ai " + String.valueOf(age) + " ans.";
// Résultat : J'ai 25 ans.
```

## Approfondissement

La concaténation de chaînes de caractères peut sembler simple, mais il est important de comprendre comment cela fonctionne en coulisses. En Java, chaque fois qu'une chaîne de caractères est concaténée, une nouvelle chaîne est créée en mémoire. Cela peut entraîner une utilisation excessive de la mémoire si la concaténation est utilisée de manière répétée dans une boucle ou avec de grandes chaînes.

Pour remédier à ce problème, il est recommandé d'utiliser la classe `StringBuilder` si vous avez besoin de concaténer plusieurs chaînes de caractères. Cette classe utilise un tampon interne et ne crée une nouvelle chaîne que lorsque cela est nécessaire, ce qui la rend plus efficace en termes de mémoire.

Enfin, il est également important de noter que la concaténation de chaînes de caractères en Java peut être assez coûteuse en termes de performances. Si vous avez besoin de concaténer un grand nombre de chaînes, il est préférable d'utiliser la classe `StringBuffer` qui est optimisée pour les opérations de concaténation.

## Voir aussi

* [Documentation officielle Java sur la concaténation de chaînes](https://docs.oracle.com/javase/tutorial/java/data/manipstrings.html)
* [Article sur les performances de la concaténation de chaînes en Java](https://www.baeldung.com/java-string-concatenation)