---
title:                "Analyser une date depuis une chaîne de caractères"
aliases:
- fr/java/parsing-a-date-from-a-string.md
date:                  2024-02-03T19:14:21.185044-07:00
model:                 gpt-4-0125-preview
simple_title:         "Analyser une date depuis une chaîne de caractères"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/java/parsing-a-date-from-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?
Analyser une date à partir d'une chaîne de caractères implique de convertir la représentation textuelle d'une date et d'une heure en un objet `Date` ou en un objet `LocalDateTime` plus moderne. Les programmeurs font cela pour manipuler, formater, comparer ou stocker des dates dans un format standardisé, ce qui est crucial pour les applications nécessitant des calculs de dates, une validation, ou une internationalisation cohérente.

## Comment faire :

### Utilisation du package `java.time` (Recommandé dans Java 8 et ultérieur) :
```java
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;

public class DateParser {
    public static void main(String[] args) {
        String dateString = "2023-04-30";
        DateTimeFormatter formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd");
        LocalDate date = LocalDate.parse(dateString, formatter);
        System.out.println(date); // Sortie: 2023-04-30
    }
}
```

### Utilisation de `SimpleDateFormat` (Approche plus ancienne) :
```java
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;

public class DateParser {
    public static void main(String[] args) {
        String dateString = "30/04/2023";
        SimpleDateFormat formatter = new SimpleDateFormat("dd/MM/yyyy");
        try {
            Date date = formatter.parse(dateString);
            System.out.println(date); // Le format de sortie dépend du format par défaut de votre système
        } catch (ParseException e) {
            e.printStackTrace();
        }
    }
}
```

### Utilisation des bibliothèques tierces (par exemple, Joda-Time) :
Joda-Time a été une importante bibliothèque tierce mais est maintenant en mode de maintenance à cause de l'introduction du package `java.time` dans Java 8. Cependant, pour ceux utilisant des versions Java antérieures à 8, Joda-Time reste un bon choix.
```java
import org.joda.time.LocalDate;
import org.joda.time.format.DateTimeFormat;
import org.joda.time.format.DateTimeFormatter;

public class DateParser {
    public static void main(String[] args) {
        String dateString = "2023-04-30";
        DateTimeFormatter formatter = DateTimeFormat.forPattern("yyyy-MM-dd");
        LocalDate date = LocalDate.parse(dateString, formatter);
        System.out.println(date); // Sortie: 2023-04-30
    }
}
```
Notez que lors du travail avec des dates, soyez toujours conscient des paramètres de fuseau horaire si vous analysez ou formatez des dates-heures plutôt que de simples dates.
