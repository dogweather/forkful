---
title:                "Java: Å beregne en dato i fremtiden eller fortiden"
simple_title:         "Å beregne en dato i fremtiden eller fortiden"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/java/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Hvorfor
Å beregne datoer i fremtiden eller fortiden kan være nyttig for ulike programmeringsprosjekter, som for eksempel å vise oppdaterte frister eller planlegge kommende arrangementer.

## Hvordan
For å beregne en dato i fremtiden eller fortiden trenger du å vite tre ting: nåværende dato, antall dager å legge til eller trekke fra, og om det er snakk om å legge til eller trekke fra.

```Java
import java.time.LocalDate;

// Sett nåværende dato
LocalDate nåværendeDato = LocalDate.now();

// Antall dager å legge til eller trekke fra
int antallDager = 10; 

// For å legge til dager:
LocalDate fremtidigDato = nåværendeDato.plusDays(antallDager);

// For å trekke fra dager:
LocalDate fortidigDato = nåværendeDato.minusDays(antallDager);

// Skriv ut resultatene
System.out.println("In the future: " + fremtidigDato);
System.out.println("In the past: " + fortidigDato);
```

Output:
```
In the future: 2021-05-27
In the past: 2021-05-07
```

Når du bruker Java's `LocalDate`-klasse, vil du få en korrekt dato uavhengig av skuddår eller månedslengder.

## Dykke dypere
Hvis du ønsker å beregne datoer i et mer spesifikt format, kan du også bruke `LocalDate` til å konstruere en dato med en bestemt årstall, måned og dag.

```Java
// Konstruerer en dato med 2022 som år, april som måned, og den 15. som dag
LocalDate datoen = LocalDate.of(2022, 04, 15);

// Skriv ut datoen
System.out.println(datoen);

// Output:
2022-04-15
```

Du kan også utføre mer kompliserte operasjoner, som å finne antall dager mellom to datoer, ved hjelp av Java's `Period`-klasse.

```Java
// Sett to datoer
LocalDate førsteDato = LocalDate.of(2020, 01, 01);
LocalDate andreDato = LocalDate.of(2020, 12, 31);

// Bruk Period klassen for å finne antall dager mellom de to datoene
Period forskjell = Period.between(førsteDato, andreDato);
int antallDager = forskjell.getDays();

// Skriv ut resultatet
System.out.println("Number of days between: " + antallDager);

// Output:
Number of days between: 364
```

## Se også
- [Java LocalDateTime API](https://docs.oracle.com/javase/8/docs/api/java/time/LocalDateTime.html)
- [Tutorial: Getting Started with Java in VS Code](https://code.visualstudio.com/docs/java/java-tutorial)
- [How to Get Current Date and Time in Java](https://www.baeldung.com/java-current-date-time)