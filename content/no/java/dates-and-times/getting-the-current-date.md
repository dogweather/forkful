---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:09:53.813741-07:00
description: "Hvordan: Java tilbyr flere m\xE5ter \xE5 f\xE5 den n\xE5v\xE6rende datoen\
  \ p\xE5, ved bruk av b\xE5de den gamle `java.util.Date` klassen og den nyere `java.time`\
  \ pakken\u2026"
lastmod: '2024-03-13T22:44:40.676871-06:00'
model: gpt-4-0125-preview
summary: "Java tilbyr flere m\xE5ter \xE5 f\xE5 den n\xE5v\xE6rende datoen p\xE5,\
  \ ved bruk av b\xE5de den gamle `java.util.Date` klassen og den nyere `java.time`\
  \ pakken (introdusert i Java 8) som er mer allsidig og intuitiv."
title: "F\xE5 dagens dato"
weight: 29
---

## Hvordan:
Java tilbyr flere måter å få den nåværende datoen på, ved bruk av både den gamle `java.util.Date` klassen og den nyere `java.time` pakken (introdusert i Java 8) som er mer allsidig og intuitiv.

### Bruk av `java.time.LocalDate`
```java
import java.time.LocalDate;

public class CurrentDateExample {
    public static void main(String[] args) {
        LocalDate currentDate = LocalDate.now();
        System.out.println(currentDate); // Eksempelutskrift: 2023-04-01
    }
}
```

### Bruk av `java.time.LocalDateTime`
```java
import java.time.LocalDateTime;

public class CurrentDateExample {
    public static void main(String[] args) {
        LocalDateTime currentDateTime = LocalDateTime.now();
        System.out.println(currentDateTime); // Eksempelutskrift: 2023-04-01T12:34:56.789
    }
}
```

### Bruk av `java.util.Date` (Legacy)
```java
import java.util.Date;

public class CurrentDateExample {
    public static void main(String[] args) {
        Date currentDate = new Date();
        System.out.println(currentDate); // Eksempelutskrift: Lør Apr 01 12:34:56 BST 2023
    }
}
```

### Bruke et bibliotek fra tredjepart: Joda-Time
Før Java 8 var Joda-Time den anerkjente standarden for dato og tid i Java. Hvis du jobber med legacy-systemer eller foretrekker Joda-Time, her er hvordan du kan bruke det til å få den nåværende datoen:
```java
import org.joda.time.LocalDate;

public class CurrentDateExample {
    public static void main(String[] args) {
        LocalDate currentDate = LocalDate.now();
        System.out.println(currentDate); // Eksempelutskrift: 2023-04-01
    }
}
```
**Merk:** Selv om `java.util.Date` og Joda-Time fortsatt brukes, anbefales `java.time` pakken for nye prosjekter på grunn av dens uforanderlighet og omfattende API for håndtering av datoer og tider.
