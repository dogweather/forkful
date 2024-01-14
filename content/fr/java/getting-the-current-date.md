---
title:                "Java: Obtenir la date actuelle"
simple_title:         "Obtenir la date actuelle"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/java/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Pourquoi 

Il est important pour les programmeurs de pouvoir obtenir la date actuelle dans leurs programmes pour diverses raisons. Cela peut être utile pour afficher la date et l'heure dans des applications, pour enregistrer des horodatages dans des fichiers ou pour comparer des dates et prendre des décisions basées sur celles-ci.

## Comment faire 

Il existe différentes façons de récupérer la date actuelle dans un programme Java, mais il y a deux méthodes principales : en utilisant la classe `Date` ou la classe `LocalDate` de la librairie `java.time`.

```Java
// Méthode 1 : en utilisant la classe Date
import java.util.Date;

Date date = new Date(); // crée un nouvel objet Date avec la date et l'heure actuelles
System.out.println(date); // affiche la date sous forme de chaîne de caractères

// Méthode 2 : en utilisant la classe LocalDate
import java.time.LocalDate;

LocalDate date = LocalDate.now(); // crée un objet LocalDate avec la date actuelle
System.out.println(date); // affiche la date au format ISO YYYY-MM-DD
```

Dans les deux cas, vous pouvez également spécifier un fuseau horaire en utilisant la classe `DateTimeZone` de la librairie `joda-time`.

```Java
// Spécifier un fuseau horaire avec la classe Date
import java.util.Date;
import org.joda.time.DateTimeZone;

Date date = new Date();
DateTimeZone timeZone = DateTimeZone.forID("Europe/Paris"); // spécifie le fuseau horaire
System.out.println(timeZone); // affiche le fuseau horaire
```

## Plongée profonde

La classe `LocalDate` offre de nombreuses méthodes pratiques pour manipuler la date, comme `plusDays(int days)` pour ajouter un certain nombre de jours à la date actuelle, ou `minusDays(int days)` pour soustraire des jours. Elle peut également être utilisée pour comparer des dates, par exemple avec `isAfter(LocalDate other)` et `isBefore(LocalDate other)`. La classe `Date` quant à elle peut être convertie en `LocalDate` en utilisant la méthode `toLocalDate()`.

Il est également possible de personnaliser le format d'affichage de la date en utilisant la classe `DateTimeFormatter` de la librairie `java.time`.

## Voir aussi

- Tutoriel sur la classe `Date` : https://www.tutorialspoint.com/java/java_date_time.htm
- Documentation sur la classe `LocalDate` : https://docs.oracle.com/javase/8/docs/api/java/time/LocalDate.html
- Guide pratique pour utiliser la classe `DateTimeFormatter` : https://www.baeldung.com/java-datetimeformatter