---
title:                "Beregning av en dato i fremtiden eller fortiden"
aliases:
- /no/java/calculating-a-date-in-the-future-or-past.md
date:                  2024-01-20T17:31:28.575657-07:00
model:                 gpt-4-1106-preview
simple_title:         "Beregning av en dato i fremtiden eller fortiden"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/java/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å regne ut en dato i fremtiden eller fortiden handler om å legge til eller trekke fra tid fra en gitt dato. Utviklere gjør dette for å håndtere frister, planlegge events eller sporingsperioder.

## Hvordan gjøre det:
```java
import java.time.LocalDate;
import java.time.temporal.ChronoUnit;

public class DateCalculator {
    public static void main(String[] args) {
        LocalDate today = LocalDate.now();
        LocalDate futureDate = today.plusWeeks(2);
        LocalDate pastDate = today.minusDays(30);

        System.out.println("I dag: " + today);
        System.out.println("Dato 2 uker fra nå: " + futureDate);
        System.out.println("Dato 30 dager siden: " + pastDate);
    }
}
```
Eksempelutdata:
```
I dag: 2023-04-01
Dato 2 uker fra nå: 2023-04-15
Dato 30 dager siden: 2023-03-02
```

## Dyp Dykk
Før Java 8, brukte utviklere `java.util.Date` og `java.util.Calendar` for dato- og tidsmanipulasjoner, men de var klønete og ikke trådsikre. Java 8 introduserte `java.time`-pakken med `LocalDate`, `LocalTime`, og `LocalDateTime`. Disse klassene er uforanderlige og trådsikre, noe som gjør dato- og tidsberegninger enklere og sikrere. Du kan også bruke `plus` og `minus`-metoder med `TemporalUnit` for å spesifisere enheten, eller `plusPeriod` og `minusDuration` for større presisjon.

Alternativene til Java's innebygde biblioteker inkluderer Joda-Time og Apache Commons Lang. Før `java.time`, var Joda-Time det foretrukne valget på grunn av sin robusthet og brukervennlighet, men nå har mange av dens funksjoner blitt inkorporert i standard Java-biblioteket.

Når det kommer til implementasjonsdetaljer, bør utviklere være oppmerksomme på tidszonehåndtering. `LocalDate` og dens søsterklasser (`LocalTime`, `LocalDateTime`) håndterer ikke tidssoner, mens `ZonedDateTime` tar hensyn til tidssonene når du utfører dato- og tidsberegninger.

## Se Også
- Java Dokumentasjon for LocalDate: https://docs.oracle.com/javase/8/docs/api/java/time/LocalDate.html
- Java Dokumentasjon for ChronoUnit: https://docs.oracle.com/javase/8/docs/api/java/time/temporal/ChronoUnit.html
- Joda-Time Hjemmeside: https://www.joda.org/joda-time/
- Apache Commons Lang: https://commons.apache.org/proper/commons-lang/
