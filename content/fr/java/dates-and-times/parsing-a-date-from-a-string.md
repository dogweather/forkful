---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:14:21.185044-07:00
description: null
lastmod: '2024-04-05T21:53:59.150258-06:00'
model: gpt-4-0125-preview
summary: ''
title: "Analyser une date depuis une cha\xEEne de caract\xE8res"
weight: 30
---

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
