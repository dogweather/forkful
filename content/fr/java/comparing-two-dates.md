---
title:                "Comparer deux dates"
html_title:           "Clojure: Comparer deux dates"
simple_title:         "Comparer deux dates"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/java/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est & Pourquoi?
Comparer deux dates permet de déterminer quelle date est antérieure, ultérieure ou si elles sont identiques. Les développeurs le font pour générer des rappels, trier des événements, calculer des délais, etc.

## Comment faire:
En java, nous pouvons utiliser la méthode `compareTo()` de la classe `Date`. Voyez l'exemple suivant:

```Java
import java.util.Date;
public class Main {
  public static void main(String[] args) {
    // Création de deux dates
    Date date1 = new Date(2020, 01, 01);
    Date date2 = new Date(2020, 01, 02);

    // Comparaison des deux dates
    int result = date1.compareTo(date2);
    
    if (result > 0) {
      System.out.println("date1 est postérieure à date2");
    } else if (result < 0) {
      System.out.println("date1 est antérieure à date2");
    } else {
      System.out.println("date1 est identique à date2");
    }
  }
}
```

La sortie potentielle sera:

```
date1 est antérieure à date2
```

## Plongée en profondeur:
Historiquement, la classe `Date` en Java a été dépréciée en faveur de `Calendar`, puis `GregorianCalendar` et actuellement `LocalDateTime`. 

Il y a plusieurs façons de comparer deux dates. Outre `compareTo()`, nous pouvons utiliser `equals()` pour vérifier l'égalité, ou `before()` et `after()` pour vérifier si une date est antérieure ou postérieure.

Lors de la comparaison de deux dates à l'aide de `compareTo()`, cette fonction renvoie `0` si les dates sont égales, une valeur inférieure à `0` si la première date est antérieure à la deuxième et une valeur supérieure à `0` si la première date est postérieure à la deuxième.

## Voir Aussi:
1. Documentation Oracle de `Date`: https://docs.oracle.com/javase/8/docs/api/java/util/Date.html
2. Cours de programmation en Java: https://www.coursera.org/specializations/java-programming
3. Tutoriels Java pour les débutant: https://www.w3schools.com/java/