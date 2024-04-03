---
date: 2024-01-20 17:33:03.571927-07:00
description: 'Comment faire : .'
lastmod: '2024-03-13T22:44:57.655412-06:00'
model: gpt-4-1106-preview
summary: .
title: Comparer deux dates
weight: 27
---

## Comment faire :
```Java
import java.time.LocalDate;
import java.time.temporal.ChronoUnit;

public class DateComparison {
    public static void main(String[] args) {
        LocalDate date1 = LocalDate.of(2023, 4, 15);
        LocalDate date2 = LocalDate.of(2023, 5, 20);
        
        // Comparer en utilisant compareTo
        int comparison = date1.compareTo(date2);

        if(comparison > 0) {
            System.out.println("Date1 est après Date2");
        } else if (comparison < 0) {
            System.out.println("Date1 est avant Date2");
        } else {
            System.out.println("Les dates sont identiques");
        }

        // Calculer la différence entre les deux dates
        long daysBetween = ChronoUnit.DAYS.between(date1, date2);
        System.out.println("Il y a " + daysBetween + " jours de différence entre les deux dates.");
    }
}
```
Sortie d'échantillon :
```
Date1 est avant Date2
Il y a 35 jours de différence entre les deux dates.
```

## Un peu plus en profondeur
Historiquement, Java utilisait `Date` et `Calendar` pour manipuler les dates, mais ces classes avaient des défauts de conception et d'utilisation. Depuis Java 8, le package `java.time` fournit une approche immuable et plus intuitive avec des classes comme `LocalDate`. Pour comparer des dates, on utilise `compareTo` ou les méthodes de `ChronoUnit`. Il existe aussi des méthodes comme `isBefore` et `isAfter` pour des vérifications plus directes. En plus, il y a `equals` pour tester l'équivalance.

## Voir aussi
- [LocalDate - Java Documentation](https://docs.oracle.com/javase/8/docs/api/java/time/LocalDate.html)
- [ChronoUnit - Java Documentation](https://docs.oracle.com/javase/8/docs/api/java/time/temporal/ChronoUnit.html)
- [Comparing Dates in Java - Baeldung](https://www.baeldung.com/java-8-date-time-intro)
