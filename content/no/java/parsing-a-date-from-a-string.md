---
title:                "Tolke en dato fra en streng"
html_title:           "Bash: Tolke en dato fra en streng"
simple_title:         "Tolke en dato fra en streng"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/java/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

# Parsing av dato fra streng i Java

## Hva og hvorfor?

Å parse en dato fra en streng er prosessen med å omforme en tekstrepresentasjon av en dato (f.eks. "01/01/2021") til en datatyperepresentasjon som Java-programmet kan behandle. Dette er nyttig når vi håndterer brukerinndata eller leser datoer fra tekstfiler.

## Hvordan:

Her er et eksempel på hvordan du parser en dato fra en streng i Java med `SimpleDateFormat`:

```java
import java.text.SimpleDateFormat;
import java.util.Date;

public class Main {
    public static void main(String[] args) {
        try {
            SimpleDateFormat format = new SimpleDateFormat("dd/MM/yyyy");
            String dateStr = "01/01/2021";
            Date parsedDate = format.parse(dateStr);
            System.out.println(parsedDate);
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}
```

Når du kjører denne koden, får du output som dette:

```shell
Fri Jan 01 00:00:00 CET 2021
```

## Dypdykk:

Historisk har Java tilbudt flere måter å parse datoer på, inkludert `Date` og `Calendar` klassene. Men disse hadde mange begrensinger og problemer, så fra Java 8 introduserte de nye Date-Time API, som `LocalDate`.

Her er et eksempel på hvordan du parser en dato med `LocalDateTime`:

```java
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;

public class Main {
    public static void main(String[] args) {
        DateTimeFormatter formatter = DateTimeFormatter.ofPattern("dd/MM/yyyy");
        String dateStr = "01/01/2021";
        LocalDateTime parsedDate = LocalDateTime.parse(dateStr, formatter);
        System.out.println(parsedDate);
    }
}
```

Koden gir denne output'en:

```shell
2021-01-01T00:00
```

`SimpleDateFormat` er ikke trådsikker, noe som betyr at du kan få problemer ved samtidig bruk i flertrådede programmer. Derfor anbefales det å bruke Date-Time API fra Java 8.

## Se også:

1. [SimpleDateFormat Java Doc](https://docs.oracle.com/javase/7/docs/api/java/text/SimpleDateFormat.html)
2. [LocalDateTime Java Doc](https://docs.oracle.com/javase/8/docs/api/java/time/LocalDateTime.html)
3. [Date-Time API Tutorial fra Oracle](https://docs.oracle.com/javase/tutorial/datetime/iso/overview.html)
4. [Official Java Tutorial - Date and Time](https://docs.oracle.com/javase/tutorial/datetime/TOC.html)