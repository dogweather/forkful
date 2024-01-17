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

## Qu'est-ce que c'est et pourquoi le faire?

Trouver la longueur d'une chaîne de caractères peut sembler être une tâche banale en programmation, mais c'est en fait une opération très importante et utile. La longueur d'une chaîne fait référence au nombre de caractères qui la composent, y compris les espaces et les symboles. Les programmeurs doivent souvent trouver la longueur d'une chaîne pour pouvoir la manipuler correctement, la comparer à d'autres chaînes ou l'afficher à l'écran.

## Comment faire:

### Exemple 1:
```Java
String phrase = "Bonjour le monde!";
System.out.println(phrase.length());
```

Output: 18

### Exemple 2:
```Java
String nom = "Marie";
System.out.println("Le nom " + nom + " a " + nom.length() + " lettres.");
```

Output: Le nom Marie a 5 lettres.

## Plongée en profondeur:

### Contexte historique:

La méthode ```.length()``` existe depuis la version 1.0 de Java et a été intégrée pour la première fois en 1996. Avant cela, les programmeurs devaient utiliser une boucle pour parcourir une chaîne et compter chaque caractère individuellement pour trouver sa longueur.

### Alternatives:

Bien qu'il existe d'autres moyens de trouver la longueur d'une chaîne en utilisant des bibliothèques externes ou en écrivant du code personnalisé, la méthode ```.length()``` reste la méthode la plus simple et la plus couramment utilisée en Java.

### Détails de mise en œuvre:

La méthode ```.length()``` est une méthode de la classe String qui renvoie un entier représentant la longueur de la chaîne. Elle peut être utilisée sur n'importe quelle chaîne de caractères, y compris les chaînes vides.

## Voir aussi:

- Documentation officielle de la méthode ```.length()```: https://docs.oracle.com/javase/7/docs/api/java/lang/String.html#length()
- Tutoriel sur les chaînes de caractères en Java: https://www.tutorialspoint.com/java/java_strings.htm