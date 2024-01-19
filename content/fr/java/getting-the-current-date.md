---
title:                "Obtenir la date actuelle"
html_title:           "Bash: Obtenir la date actuelle"
simple_title:         "Obtenir la date actuelle"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/java/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Qu'est-ce et Pourquoi?
Obtenir la date actuelle en Java se réfère simplement à recevoir la date du moment où le programme s'exécute. Les programmeurs l'utilisent souvent pour suivre les événements, les journaux d'audit ou simplement pour ajouter des informations temporelles aux données.

## Comment faire:
La classe `LocalDate` de Java, qui fait partie de l'API `java.time`, est couramment utilisée pour obtenir la date actuelle. Voici comment:

```Java
import java.time.LocalDate;

public class Main {
    public static void main(String[] args) {
        LocalDate currentDate = LocalDate.now();
        System.out.println("La date actuelle est: " + currentDate);
    }
}
```

Lors de l'exécution, ce code renvoie quelque chose comme:

```Java
La date actuelle est: 2022-03-28
```

## Plongée Profonde
Historiquement, les programmeurs utilisaient la classe `Date` de `java.util`. Cependant, à partir de Java 8, l'API Date/Time a connu une refonte majeure améliorant sa lisibilité et sa précision.

En alternative, `LocalDateTime` peut être utilisé pour obtenir l'heure en plus de la date. Son utilisation est presque identique à `LocalDate` :

```Java
import java.time.LocalDateTime;

public class Main {
    public static void main(String[] args) {
        LocalDateTime currentDateTime = LocalDateTime.now();
        System.out.println("La date et l'heure actuelles sont: " + currentDateTime);
    }
}
```

Rappelez-vous, `java.time` est immuable et thread-safe, contrairement à `java.util.Date`, ce qui le rend plus sûre d'utilisation dans un environnement multi-thread.

## Voir Aussi
1. Documentation officielle de Java sur `java.time`: https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html
2. Guide Oracle sur le travail avec les dates et les heures: https://www.oracle.com/technical-resources/articles/java/jf14-date-time.html
3. Une intro de Baeldung à 'java.time': https://www.baeldung.com/java-8-date-time-intro