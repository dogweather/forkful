---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:09:41.826776-07:00
description: "Wie: Java bietet mehrere M\xF6glichkeiten, das aktuelle Datum abzurufen,\
  \ sowohl mit der alten `java.util.Date`-Klasse als auch mit dem neueren\u2026"
lastmod: '2024-03-13T22:44:53.772974-06:00'
model: gpt-4-0125-preview
summary: "Java bietet mehrere M\xF6glichkeiten, das aktuelle Datum abzurufen, sowohl\
  \ mit der alten `java.util.Date`-Klasse als auch mit dem neueren `java.time`-Paket\
  \ (eingef\xFChrt in Java 8), das vielseitiger und intuitiver ist."
title: Den aktuellen Datum abrufen
weight: 29
---

## Wie:
Java bietet mehrere Möglichkeiten, das aktuelle Datum abzurufen, sowohl mit der alten `java.util.Date`-Klasse als auch mit dem neueren `java.time`-Paket (eingeführt in Java 8), das vielseitiger und intuitiver ist.

### Verwendung von `java.time.LocalDate`
```java
import java.time.LocalDate;

public class CurrentDateExample {
    public static void main(String[] args) {
        LocalDate currentDate = LocalDate.now();
        System.out.println(currentDate); // Beispiel Ausgabe: 2023-04-01
    }
}
```

### Verwendung von `java.time.LocalDateTime`
```java
import java.time.LocalDateTime;

public class CurrentDateExample {
    public static void main(String[] args) {
        LocalDateTime currentDateTime = LocalDateTime.now();
        System.out.println(currentDateTime); // Beispiel Ausgabe: 2023-04-01T12:34:56.789
    }
}
```

### Verwendung von `java.util.Date` (Legacy)
```java
import java.util.Date;

public class CurrentDateExample {
    public static void main(String[] args) {
        Date currentDate = new Date();
        System.out.println(currentDate); // Beispiel Ausgabe: Sa, Apr 01 12:34:56 BST 2023
    }
}
```

### Verwendung einer Drittanbieterbibliothek: Joda-Time
Vor Java 8 war Joda-Time der de-facto Standard für Datum und Zeit in Java. Wenn Sie an älteren Systemen arbeiten oder eine Vorliebe für Joda-Time haben, hier ist, wie Sie es verwenden können, um das aktuelle Datum zu erhalten:
```java
import org.joda.time.LocalDate;

public class CurrentDateExample {
    public static void main(String[] args) {
        LocalDate currentDate = LocalDate.now();
        System.out.println(currentDate); // Beispiel Ausgabe: 2023-04-01
    }
}
```
**Hinweis:** Obwohl `java.util.Date` und Joda-Time immer noch verwendet werden, wird für neue Projekte das `java.time`-Paket aufgrund seiner Unveränderlichkeit und umfassenden API zur Handhabung von Daten und Zeiten empfohlen.
