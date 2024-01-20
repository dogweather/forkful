---
title:                "Trouver la longueur d'une chaîne"
html_title:           "Go: Trouver la longueur d'une chaîne"
simple_title:         "Trouver la longueur d'une chaîne"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/java/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

# Trouver la longueur d'une chaîne en Java

## Qu'est-ce que c'est et pourquoi?

La détermination de la longueur d'une chaîne en Java est le processus qui consiste à configurer le nombre de caractères dans une chaîne donnée. Les programmeurs le font pour manipuler, valider ou comparer des données textuelles en fonction de leur longueur.

## Comment faire:

Voici un exemple simple de la façon dont c'est fait en Java :

```Java
public class Main {
    public static void main(String[] args) {
        String chaine = "Bonjour, le monde!";
        int longueur = chaine.length();
        System.out.println("La longueur de la chaine est: " + longueur);
    }
}
```

Output:

```
La longueur de la chaine est: 18
```

## Plongée profonde

Historiquement, la méthode `length()` a été implémentée dans le JDK 1.0, ce qui fait d'elle une des plus anciennes caractéristiques de Java. Il n'existe pas d'alternative directe à cette méthode dans la bibliothèque standard de Java. 

Cependant, certaines librairies externes fournissent des méthodes similaires avec des fonctionnalités supplémentaires, comme Apache’s `StringUtils.length()`, qui est null-safe et retourne 0 pour une entrée null. 

La méthode `length()` est un membre de la classe `String` en Java et elle renvoie la longueur de la chaîne courante. Elle utilise une variable de type `int` pour stocker le nombre de caractères Unicode (16 bits) qui composent la chaîne.

## Voir aussi

Pour plus d'informations sur le traitement des chaînes en Java, consultez les ressources suivantes :

- [Documentation Oracle sur les Strings](https://docs.oracle.com/en/java/javase/16/docs/api/java.base/java/lang/String.html)
- [Apache Commons Lang StringUtils](https://commons.apache.org/proper/commons-lang/javadocs/api-release/org/apache/commons/lang3/StringUtils.html)