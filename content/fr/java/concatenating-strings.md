---
title:                "Concaténation de chaînes"
html_title:           "C: Concaténation de chaînes"
simple_title:         "Concaténation de chaînes"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/java/concatenating-strings.md"
---

{{< edit_this_page >}}

## Qu'est-ce & pourquoi ?

La concaténation de chaînes en Java est l'opération d'ajout ou de liaison de deux chaînes pour former une seule. C'est pratique pour générer des messages dynamiques ou assembler des fragments de données.

## Comment faire :

Bon, voyons comment faire cela dans le code. 

```Java
public class ConcatExample {
    public static void main(String[] args) {
        String firstName = "Jean";
        String lastName = "Dupont";
        
        String fullName = firstName + " " + lastName;
        System.out.println(fullName);  // Affiche "Jean Dupont"
        
        String sentence = "Mon nom est " + fullName + ".";
        System.out.println(sentence);  // Affiche "Mon nom est Jean Dupont."
    }
}
```

Vous pouvez aussi utiliser la méthode `concat()` :

```Java
String firstName = "Jean";
String lastName = "Dupont";

String fullName = firstName.concat(" ").concat(lastName);
System.out.println(fullName);  // Affiche "Jean Dupont"
```

## Exploration approfondie

Historiquement, la concaténation de chaînes en Java était problématique car elle pouvait générer beaucoup d'objets 'String' indésirables. Depuis Java 5, la classe `StringBuilder` est utilisée en interne pour optimiser cela.

Dans certains cas, vous pouvez préférer utiliser `StringBuilder` ou `StringBuffer`. Ces classes offrent plus de contrôle, mais sont plus verbales :

```Java
StringBuilder sb = new StringBuilder();
sb.append("Mon nom est ");
sb.append(fullName);
sb.append(".");
System.out.println(sb.toString());  // Affiche "Mon nom est Jean Dupont."
```

## Voir aussi

* Documentation Oracle sur [`String`](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html)
* Documentation Oracle sur [`StringBuilder`](https://docs.oracle.com/javase/8/docs/api/java/lang/StringBuilder.html)
* Article StackOverflow sur [la concaténation de chaînes en Java](https://stackoverflow.com/questions/4645020/when-to-use-stringbuilder-in-java)