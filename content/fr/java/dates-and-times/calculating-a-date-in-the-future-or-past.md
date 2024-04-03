---
date: 2024-01-20 17:31:23.368084-07:00
description: "Calculer une date dans le futur ou le pass\xE9, c'est trouver une date\
  \ avant ou apr\xE8s une date donn\xE9e. Les programmeurs le font souvent pour des\u2026"
lastmod: '2024-03-13T22:44:57.656458-06:00'
model: gpt-4-1106-preview
summary: "Calculer une date dans le futur ou le pass\xE9, c'est trouver une date avant\
  \ ou apr\xE8s une date donn\xE9e."
title: "Calcul d'une date future ou pass\xE9e"
weight: 26
---

## Comment faire :
```java
import java.time.LocalDate;
import java.time.temporal.ChronoUnit;

public class DateFuturePast {
    public static void main(String[] args) {
        LocalDate today = LocalDate.now();
        LocalDate tenDaysLater = today.plusDays(10);
        LocalDate thirtyDaysAgo = today.minusDays(30);

        System.out.println("Aujourd'hui: " + today);
        System.out.println("Dans 10 jours: " + tenDaysLater);
        System.out.println("Il y a 30 jours: " + thirtyDaysAgo);
    }
}
```
Sortie:
```
Aujourd'hui: 2023-03-15
Dans 10 jours: 2023-03-25
Il y a 30 jours: 2023-02-13
```

## Approfondissements
Historiquement, la gestion des dates en Java était complexe et imparfaite avec les classes `Date` et `Calendar`. Java 8 a introduit l'API `java.time`, plus robuste et intuitive. Pour calculer une date dans le futur ou le passé, utilisez l'API `java.time` avec des méthodes comme `plusDays` ou `minusDays`.

Il existe d'autres alternatives comme Joda-Time, mais depuis Java 8, elle est moins utilisée car `java.time` comble la plupart des besoins. Pour des calculs plus complexes, par exemple en ignorant les weekends ou les jours fériés, il faudra implémenter une logique supplémentaire.

## Voir Aussi
- [Documentation de l'API java.time](https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html)
- [Guide Oracle sur les dates et heures](https://docs.oracle.com/javase/tutorial/datetime/)
- [Les nouveautés de Java 8](https://www.oracle.com/technetwork/java/javase/8-whats-new-2157071.html)
