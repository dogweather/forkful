---
title:                "Obtenir la date actuelle"
date:                  2024-01-20T15:15:03.959181-07:00
simple_title:         "Obtenir la date actuelle"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/java/getting-the-current-date.md"
---

{{< edit_this_page >}}

## What & Why?
Obtenir la date actuelle en Java, c'est lire l'horloge système pour connaître le moment présent. On le fait pour des raisons telles que l'enregistrement de logs, les timestamps de transactions ou encore la planification d’événements futurs.

## How to:
Voici comment obtenir la date du jour en Java :

```java
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;

public class GetCurrentDate {
    public static void main(String[] args) {
        LocalDate today = LocalDate.now();
        System.out.println("La date d'aujourd'hui est : " + today);

        // Bonus: formater la date
        DateTimeFormatter formatter = DateTimeFormatter.ofPattern("dd/MM/yyyy");
        String formattedDate = today.format(formatter);
        System.out.println("Formatée, cela donne : " + formattedDate);
    }
}
```

Sortie :
```
La date d'aujourd'hui est : 2023-04-12
Formatée, cela donne : 12/04/2023
```

## Deep Dive
Avant Java 8, on utilisait `java.util.Date` ou `java.util.Calendar` pour les dates, mais ces classes avaient des problèmes: elles n'étaient pas thread-safe et leur utilisation était compliquée. Java 8 a introduit `java.time`, le package qui simplifie la gestion du temps et est inspiré de Joda-Time. `LocalDate.now()`que nous avons utilisé est thread-safe et immuable, ce qui est bien vu côté sécurité. Pour des formats précis, on utilise `DateTimeFormatter`. Si tu as besoin de l'heure en plus, jette un oeil à `LocalDateTime`.

## See Also
Pour approfondir ou explorer des cas spécifiques :

- La documentation oracle sur `java.time`: https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html
- Un guide sur `DateTimeFormatter`: https://docs.oracle.com/javase/8/docs/api/java/time/format/DateTimeFormatter.html
- Tutoriel complet sur la date et l'heure en Java: https://www.baeldung.com/java-8-date-time-intro
