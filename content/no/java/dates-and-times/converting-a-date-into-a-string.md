---
date: 2024-01-20 17:36:52.201626-07:00
description: "How to: (Slik gj\xF8r du det:) I moderne Java bruker vi `java.time.format.DateTimeFormatter`\
  \ sammen med `java.time.LocalDate` eller `java.time.LocalDateTime`\u2026"
lastmod: '2024-04-05T21:53:41.658318-06:00'
model: gpt-4-1106-preview
summary: "(Slik gj\xF8r du det:) I moderne Java bruker vi `java.time.format.DateTimeFormatter`\
  \ sammen med `java.time.LocalDate` eller `java.time.LocalDateTime` for datokonvertering."
title: Konvertere en dato til en streng
weight: 28
---

## How to: (Slik gjør du det:)
I moderne Java bruker vi `java.time.format.DateTimeFormatter` sammen med `java.time.LocalDate` eller `java.time.LocalDateTime` for datokonvertering:

```java
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;

public class DateToStringExample {
    public static void main(String[] args) {
        LocalDate today = LocalDate.now();
        DateTimeFormatter formatter = DateTimeFormatter.ofPattern("dd.MM.yyyy");
        String formattedDate = today.format(formatter);
        
        System.out.println(formattedDate); // Output: "dd.MM.yyyy"-formatert dagens dato
    }
}
```

## Deep Dive (Dypdykk)
Før Java 8 brukte vi `SimpleDateFormat` fra `java.util`-pakken, men det hadde trådsikkerhetsproblemer. Med `DateTimeFormatter` fra `java.time`-pakken (introdusert i Java 8) oppnår vi både trådsikkerhet og bedre API-design. Forskjellige `DateTimeFormatter`-mønstre lar oss tilpasse strengutseendet. Alternativt kan vi bruke forhåndsdefinerte formater som `DateTimeFormatter.ISO_LOCAL_DATE`.

Når det gjelder ytelse og minnebruk, er de nye dato- og tidsklassene mer effektive enn de gamle. De støtter også internasjonalisering med `Locale`.

## See Also (Se Også)
- Offisiell dokumentasjon for `DateTimeFormatter`: https://docs.oracle.com/javase/8/docs/api/java/time/format/DateTimeFormatter.html
- Oracle tutorial om dato- og tid-API: https://docs.oracle.com/javase/tutorial/datetime/
- Stack Overflow diskusjoner om dato-til-streng-konvertering i Java: https://stackoverflow.com/questions/tagged/java+date+string
