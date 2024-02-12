---
title:                "Den aktuellen Datum abrufen"
aliases:
- /de/java/getting-the-current-date/
date:                  2024-02-03T19:09:41.826776-07:00
model:                 gpt-4-0125-preview
simple_title:         "Den aktuellen Datum abrufen"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/java/getting-the-current-date.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Was & Warum?
Das Abrufen des aktuellen Datums in Java ist eine grundlegende Operation, die es Programmierern ermöglicht, Datum-Objekte für Operationen wie Protokollierung, Datumsberechnungen und zeitbasierte Bedingungen zu manipulieren. Es ist von entscheidender Bedeutung in Anwendungen, bei denen das Verfolgen, Planen und die Analyse zeitlicher Daten entscheidend sind.

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
