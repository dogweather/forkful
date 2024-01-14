---
title:    "Java: Calculer une date dans le futur ou le passé"
keywords: ["Java"]
---

{{< edit_this_page >}}

# Pourquoi 

La programmation Java offre de nombreuses fonctionnalités pour calculer des dates dans le passé ou dans le futur. Cela peut être utile dans de nombreux cas, tels que la planification d'événements ou la gestion de tâches.

## Comment faire

Pour calculer une date dans le futur ou dans le passé, nous utiliserons la classe `Calendar` de Java. Voici un exemple de code pour calculer une date dans le futur et afficher le résultat :

```
Java
Calendar cal = Calendar.getInstance();
cal.add(Calendar.DATE, 7);
System.out.println(cal.getTime());
```

Cela ajoutera 7 jours à la date actuelle et affichera la nouvelle date dans le format suivant : `Sun Sep 06 00:00:00 GMT 2020`

Pour calculer une date dans le passé, nous utilisons la méthode `set` pour modifier les valeurs du jour, du mois et de l'année. Voici un exemple de code pour calculer une date dans le passé et afficher le résultat :

```
Java
Calendar cal = Calendar.getInstance();
cal.set(Calendar.DAY_OF_MONTH, 23);
cal.set(Calendar.MONTH, Calendar.DECEMBER);
cal.set(Calendar.YEAR, 2019);
System.out.println(cal.getTime());
```

Cela définira la date au 23 décembre 2019 et affichera le résultat dans le même format que précédemment.

## Plongée en profondeur

En plus de la classe `Calendar`, Java offre également la classe `LocalDate` pour la gestion des dates. Elle présente des avantages tels que la prise en compte des fuseaux horaires et la prise en charge de dates antérieures à 1583.

Pour calculer une date dans le futur ou dans le passé avec `LocalDate`, nous pouvons utiliser les méthodes `plusDays()` et `minusDays()`. Voici un exemple de code :

```
Java
LocalDate date = LocalDate.now();
System.out.println(date.plusDays(30));
System.out.println(date.minusDays(10));
```

Ces méthodes renverront des objets `LocalDate` modifiés avec les nouvelles dates calculées.

## Voir aussi

- [Documentation officielle de Java pour la classe Calendar](https://docs.oracle.com/javase/8/docs/api/java/util/Calendar.html)
- [Documentation officielle de Java pour la classe LocalDate](https://docs.oracle.com/javase/8/docs/api/java/time/LocalDate.html)
- [Tutoriel sur la gestion des dates en Java](https://www.tutorialspoint.com/java/java_date_time.htm)