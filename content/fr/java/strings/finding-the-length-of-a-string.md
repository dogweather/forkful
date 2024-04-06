---
date: 2024-01-20 17:47:38.681256-07:00
description: 'Comment faire : Sortie .'
lastmod: '2024-04-05T21:53:59.132146-06:00'
model: gpt-4-1106-preview
summary: ''
title: "Trouver la longueur d'une cha\xEEne de caract\xE8res"
weight: 7
---

## Comment faire :
```java
public class LongueurChaine {
    public static void main(String[] args) {
        String chaine = "Bonjour le monde!";
        int longueur = chaine.length();
        System.out.println("La longueur de la chaîne est : " + longueur);
    }
}
```
Sortie :
```
La longueur de la chaîne est : 17
```

## Un Peu Plus Loin
Historiquement, la méthode `.length()` fait partie de Java depuis ses débuts. C'est la manière la plus directe pour obtenir la taille d'une chaîne de caractères. Des alternatives comme `String.toCharArray().length` ou `StringUtils.length()` d'Apache Commons Lang existent, mais elles sont moins directes et parfois plus coûteuses en performance. Sous le capot, `.length()` accède simplement à un champ de l'objet `String` qui garde le compte des caractères, donc cela ne nécessite pas de calculer la longueur à chaque appel.

## Voir Également
- La documentation officielle de la classe [String](https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/lang/String.html) pour plus de méthodes et de détails.
- [Apache Commons Lang StringUtils](https://commons.apache.org/proper/commons-lang/javadocs/api-release/org/apache/commons/lang3/StringUtils.html) pour des méthodes d'aide sur les chaînes de caractères.
- Un guide sur les [chaînes de caractères en Java](https://www.baeldung.com/java-string) chez Baeldung pour explorer plus de concepts liés aux chaînes.
