---
title:                "Java: Calculer une date dans le futur ou le pass"
programming_language: "Java"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/java/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Pourquoi

Il est parfois nécessaire de calculer une date dans le futur ou dans le passé dans un programme Java. Cela peut être utile pour des tâches telles que la planification d'événements ou la gestion des alarmes.

## Comment faire

Voici un exemple de code Java pour calculer une date future en utilisant la classe `LocalDate` de la librairie standard :

```Java
import java.time.LocalDate;

// Date actuelle
LocalDate dateActuelle = LocalDate.now();

// Ajouter 10 jours à la date actuelle
LocalDate dateFuture = dateActuelle.plusDays(10);

// Afficher la date future dans un format spécifique
System.out.println("Dans 10 jours, ce sera le " + dateFuture.format(DateTimeFormatter.ofPattern("dd MMMM yyyy")));
```

Output :

```
Dans 10 jours, ce sera le 23 septembre 2021
```

Pour calculer une date dans le passé, il suffit d'utiliser la méthode `minus` au lieu de `plus` :

```Java
// Date actuelle
LocalDate dateActuelle = LocalDate.now();

// Soustraire 1 mois à la date actuelle
LocalDate datePassee = dateActuelle.minusMonths(1);

// Afficher la date passée dans un format spécifique
System.out.println("Il y a un mois, nous étions le " + datePassee.format(DateTimeFormatter.ofPattern("dd MMMM yyyy")));
```

Output :

```
Il y a un moins, nous étions le 28 août 2021
```

## Plongée profonde

Bien que les exemples ci-dessus soient simples, la librairie de date et de temps de Java offre de nombreuses fonctionnalités pour calculer des dates dans le futur ou dans le passé. Par exemple, vous pouvez utiliser des périodes (`Period`) pour ajouter ou soustraire des jours, des mois et des années à une date, ou encore des intervalles (`Duration`) pour gérer des dates et heures précises.

Il existe également des classes spécialisées pour la manipulation de dates selon différents calendriers, tels que le calendrier grégorien et le calendrier julien.

Pour en savoir plus sur les différentes possibilités de la librairie de date et de temps de Java, vous pouvez consulter la documentation officielle : [https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/time/package-summary.html](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/time/package-summary.html).

## À voir aussi
- [https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/time/package-summary.html](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/time/package-summary.html)
- [https://www.baeldung.com/java-8-date-time-intro](https://www.baeldung.com/java-8-date-time-intro)
- [https://openclassrooms.com/fr/courses/26832-apprenez-a-programmer-en-java/26323-les-dates](https://openclassrooms.com/fr/courses/26832-apprenez-a-programmer-en-java/26323-les-dates)