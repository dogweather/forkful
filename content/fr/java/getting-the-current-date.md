---
title:                "Java: Obtenir la date actuelle"
programming_language: "Java"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/java/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Pourquoi

Obtenir la date actuelle est une tâche commune en programmation. Cela peut être utile pour afficher la date dans un format spécifique dans une application ou pour exécuter des tâches basées sur une certaine date. Dans cet article, nous allons explorer comment obtenir la date actuelle en Java.

## Comment faire

Pour obtenir la date actuelle en Java, nous pouvons utiliser la classe "LocalDate" du package "java.time". Cette classe représente une date sans information sur l'heure ou le fuseau horaire. Voici un exemple de code pour obtenir la date actuelle et l'afficher dans la console :

```Java
import java.time.LocalDate;

public class CurrentDateExample {
    public static void main(String[] args) {
        // Obtenir la date actuelle
        LocalDate now = LocalDate.now();
        
        // Afficher la date dans le format "YYYY-MM-JJ"
        System.out.println("La date actuelle est : " + now);
    }
}
```

La sortie de ce code sera :

```text
La date actuelle est : 2021-09-22
```

Nous pouvons également spécifier un fuseau horaire pour obtenir la date dans cette zone spécifique. Par exemple, si nous voulons obtenir la date actuelle dans le fuseau horaire "America/New_York", nous pouvons utiliser la classe "ZoneId" et la méthode "of()" pour spécifier le fuseau horaire. Voici un exemple de code pour cela :

```Java
import java.time.LocalDate;
import java.time.ZoneId;

public class CurrentDateExample {
    public static void main(String[] args) {
        // Spécifier le fuseau horaire
        ZoneId zoneId = ZoneId.of("America/New_York");
        
        // Obtenir la date actuelle dans le fuseau horaire spécifié
        LocalDate nowInNY = LocalDate.now(zoneId);
        
        // Afficher la date dans le format "YYYY-MM-JJ"
        System.out.println("La date actuelle à New York est : " + nowInNY);
    }
}
```

La sortie de ce code sera :

```text
La date actuelle à New York est : 2021-09-21
```

## Plongée en profondeur

Maintenant que nous savons comment obtenir la date actuelle en Java, nous pouvons approfondir notre compréhension en explorant les différentes méthodes disponibles dans la classe "LocalDate". Voici quelques-unes des méthodes utiles pour travailler avec une date :

- `getYear()` : retourne l'année en cours dans la date.
- `getMonth()` : retourne le mois en cours dans la date.
- `getDayOfMonth()` : retourne le jour du mois en cours dans la date.
- `isLeapYear()` : vérifie si l'année en cours est une année bissextile.
- `plusDays()` : ajoute un nombre spécifié de jours à la date.


## Voir aussi

- Tutoriel sur la classe "LocalDate" : https://www.baeldung.com/java-localdate
- Documentation officielle sur la classe "LocalDate" : https://docs.oracle.com/javase/8/docs/api/java/time/LocalDate.html
- Article sur l'utilisation correcte des fuseaux horaires en Java : https://blogs.oracle.com/corejavatechtips/how-to-correctly-use-time-zones-in-java-8