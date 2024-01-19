---
title:                "Convertir une chaîne en minuscules"
html_title:           "Arduino: Convertir une chaîne en minuscules"
simple_title:         "Convertir une chaîne en minuscules"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/java/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?
La conversion d'une chaîne en minuscules est une pratique courante en Java. C'est utile pour la normalisation des données, permettant une comparaison cohérente entre les chaînes, indépendamment de la casse.

## Comment faire :
Voici un exemple assez simple :
```Java
public class Main {
    public static void main(String[] args) {
        String text = "Bonjour tout le Monde!";
        String textEnMinuscule = text.toLowerCase();
        System.out.println(textEnMinuscule);
    }
}
```
Ce code affiche : `bonjour tout le monde!` 

## Plongeons plus profondément
Historiquement, les opérations de casse sont cruciales dans de nombreux systèmes tels que les bases de données, pour assurer l'uniformité lors du stockage ou de la recherche d'information. 

Une autre alternative pourrait être d'utiliser `equalsIgnoreCase()`, qui compare deux chaînes en ignorant la casse. Cependant, sa portée est limitée à la comparaison et elle ne convertit pas les chaînes en minuscules.

Les détails de l'implémentation de la méthode `toLowerCase()` en Java dépendent de la localisation. Par exemple, pour les langues qui ont des caractères spéciaux ou des règles de casse spécifiques, la méthode `toLowerCase(Locale locale)` serait plus appropriée.

## Voir aussi :
1. [Java String toLowerCase() Method - w3schools](https://www.w3schools.com/java/ref_string_tolowercase.asp)
2. [Java – Convert String to lower case - mkyong](https://mkyong.com/java/java-convert-string-to-lower-case/)
3. [String toLowerCase() function in Java with Examples - GeeksforGeeks](https://www.geeksforgeeks.org/string-tolowercase-function-in-java-with-examples/)