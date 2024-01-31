---
title:                "Slik får du tak i dagens dato"
date:                  2024-01-20T15:15:13.150051-07:00
simple_title:         "Slik får du tak i dagens dato"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/java/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å hente dagens dato i Java betyr ganske enkelt å få tak i den nåværende datoen fra systemet. Programmerere bruker dette for å loggføre hendelser, håndtere tidsfrister eller vise tid til brukere.

## Slik gjør du:
```java
import java.time.LocalDate;

public class Main {
    public static void main(String[] args) {
        LocalDate today = LocalDate.now();
        System.out.println("Dagens dato er: " + today);
    }
}
```

Kjører du denne, får du en utskrift som:
```
Dagens dato er: 2023-04-12
```

## Dypdykk:
Før Java 8, var `java.util.Date` og `java.util.Calendar` de gå-til bibliotekene for dato og tid. Men de hadde sine mangler – forvirrende API og mangler tidssone-støtte. Java 8 introduserte `java.time`-pakken, kalt DateTime API, som løste disse problemene. Alternativer innebærer bruk av tredjeparts biblioteker som Joda-Time, men siden Java 8 er de for det meste overflødige. Når du henter dagens dato med `LocalDate.now()`, bruker du system-klokken etter forhåndsinnstilt tidssone. Vil du ha kontroll over tidssonen, bruk `ZonedDateTime` eller `OffsetDateTime`.

## Se Også:
- [JavaDoc for LocalDate](https://docs.oracle.com/javase/8/docs/api/java/time/LocalDate.html)
- [Oracle tutorial on Date Time](https://docs.oracle.com/javase/tutorial/datetime/)
- [Baeldung on the Java 8 Date/Time API](https://www.baeldung.com/java-8-date-time-intro)
