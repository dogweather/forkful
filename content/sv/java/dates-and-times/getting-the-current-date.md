---
title:                "Få det aktuella datumet"
aliases:
- /sv/java/getting-the-current-date/
date:                  2024-02-03T19:09:49.328780-07:00
model:                 gpt-4-0125-preview
simple_title:         "Få det aktuella datumet"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/java/getting-the-current-date.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Vad & Varför?
Att erhålla dagens datum i Java är en grundläggande operation som tillåter programmerare att manipulera datumobjekt för operationer såsom loggning, datumberäkningar och tidsbaserade villkor. Det är avgörande i applikationer där spårning, schemaläggning och analys av tidsdata är kritiskt.

## Hur man gör:
Java erbjuder flera sätt att få dagens datum, genom att använda både den gamla `java.util.Date`-klassen och det nyare `java.time`-paketet (introducerat i Java 8) som är mer mångsidigt och intuitivt.

### Använda `java.time.LocalDate`
```java
import java.time.LocalDate;

public class CurrentDateExample {
    public static void main(String[] args) {
        LocalDate currentDate = LocalDate.now();
        System.out.println(currentDate); // Exempelutskrift: 2023-04-01
    }
}
```
### Använda `java.time.LocalDateTime`
```java
import java.time.LocalDateTime;

public class CurrentDateExample {
    public static void main(String[] args) {
        LocalDateTime currentDateTime = LocalDateTime.now();
        System.out.println(currentDateTime); // Exempelutskrift: 2023-04-01T12:34:56.789
    }
}
```
### Använda `java.util.Date` (Äldre)
```java
import java.util.Date;

public class CurrentDateExample {
    public static void main(String[] args) {
        Date currentDate = new Date();
        System.out.println(currentDate); // Exempelutskrift: Lör Apr 01 12:34:56 BST 2023
    }
}
```
### Använda ett tredjepartbibliotek: Joda-Time
Innan Java 8 var Joda-Time den de facto-standarden för datum och tid i Java. Om du arbetar med äldre system eller föredrar Joda-Time, här är hur du kan använda det för att få dagens datum:
```java
import org.joda.time.LocalDate;

public class CurrentDateExample {
    public static void main(String[] args) {
        LocalDate currentDate = LocalDate.now();
        System.out.println(currentDate); // Exempelutskrift: 2023-04-01
    }
}
```
**Notera:** Medan `java.util.Date` och Joda-Time fortfarande används, rekommenderas `java.time`-paketet för nya projekt på grund av dess oföränderlighet och omfattande API för att hantera datum och tider.
