---
title:                "Java: Comparaison de deux dates"
programming_language: "Java"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/java/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Pourquoi

La comparaison de dates est un aspect crucial de la programmation Java. Cela permet aux développeurs de vérifier si une date est avant, après ou égale à une autre date. Cela peut également être utile pour trier les données en fonction des dates. Dans cet article, nous allons explorer comment comparer deux dates en utilisant Java.

## Comment faire

Pour comparer deux dates en Java, nous pouvons utiliser la méthode `compareTo()` de la classe `Date`. Cette méthode renvoie un entier positif si la première date est après la seconde, un entier négatif si la première date est avant la seconde, et 0 si les deux dates sont égales.

Voici un exemple de code montrant comment utiliser la méthode `compareTo()` pour comparer deux dates :

```Java
import java.util.Date;

public class ComparerDates {

    public static void main(String[] args) {

        // Créer deux objets Date
        Date date1 = new Date(2021, 4, 15);
        Date date2 = new Date(2021, 4, 20);

        // Comparer les dates
        int resultat = date1.compareTo(date2);

        // Afficher le résultat
        if (resultat > 0) {
            System.out.println("La date 1 est après la date 2");
        } else if (resultat < 0) {
            System.out.println("La date 1 est avant la date 2");
        } else {
            System.out.println("Les deux dates sont égales");
        }
    }
}
```

### Sortie :

```
La date 1 est avant la date 2
```

## Plongée dans les détails

En utilisant la méthode `compareTo()`, la comparaison de deux dates se fait en analysant les millisecondes depuis le 1er janvier 1970. Cela signifie que si les deux dates ont la même année, le même mois et le même jour, mais des heures différentes, la date avec l'heure la plus récente sera considérée comme après l'autre.

De plus, la classe `Date` a été déclarée obsolète depuis Java 8 et remplacée par la classe `LocalDate` de la bibliothèque `java.time`. Cette classe offre des méthodes plus avancées pour comparer des dates, telles que `isAfter()`, `isBefore()` et `isEqual()`.

## Voir aussi

- Documentation officielle de la méthode `compareTo()` de la classe `Date` : https://docs.oracle.com/javase/8/docs/api/java/util/Date.html#compareTo-java.util.Date-
- Tutoriel sur les dates en Java : https://www.javatpoint.com/java-date
- Documentation officielle de la classe `LocalDate` de la bibliothèque `java.time` : https://docs.oracle.com/javase/8/docs/api/java/time/LocalDate.html