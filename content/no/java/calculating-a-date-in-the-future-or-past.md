---
title:                "Beregning av datoer i fremtiden eller fortiden"
html_title:           "Java: Beregning av datoer i fremtiden eller fortiden"
simple_title:         "Beregning av datoer i fremtiden eller fortiden"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/java/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Hvorfor

Å kunne beregne en dato i fremtiden eller fortiden kan være nyttig for å planlegge hendelser eller for å kontrollere at dataen i et program er nøyaktig. Det kan også være en nyttig ferdighet å ha i en jobb som involverer tidssensitive oppgaver.

## Hvordan

For å beregne en dato i Java, kan du bruke klassen `Calendar`. Her er et eksempel på hvordan du kan beregne en dato 5 dager frem i tid fra i dag:

```Java
// Oppretter en Calendar-instans med dagens dato
Calendar cal = Calendar.getInstance();

// Legger til 5 dager til dagens dato
cal.add(Calendar.DAY_OF_MONTH, 5);

// Henter ut datoen og skriver den ut
Date futureDate = cal.getTime();
System.out.println("Fremtidig dato: " + futureDate);
```

Eksempel på output: `Fremtidig dato: Wed Jul 14 16:25:49 CEST 2021`

Hvis du heller vil beregne en dato i fortiden, kan du bruke `Calendar`'s `set`-metode og angi et negativt tall som parameter:

```Java
// Setter kalenderen til å være -2 uker fra dagens dato
cal.set(Calendar.WEEK_OF_MONTH, -2);
```

Eksempel på output: `Fortidig dato: Sun Jun 27 16:25:49 CEST 2021`

Det er også mulig å beregne en dato ved å bruke `Date`-klassen og konvertere den til en `Calendar`-instans:

```Java
// Oppretter en Date instans med datoen 2. august 2021
Date date = new Date(121, 7, 2);

// Konverterer til Calendar-instans
cal.setTime(date);

// Henter ut datoen og skriver den ut
Date futureDate = cal.getTime();
System.out.println("Fremtidig dato: " + futureDate);
```

Eksempel på output: `Fremtidig dato: Mon Aug 02 16:25:49 CEST 2021`

## Dypdykk

Når du bruker `Calendar` til å beregne datoer, er det viktig å merke seg at måneder begynner på indeks 0, mens dager i uken begynner på indeks 1. Dette kan føre til uforventede resultater hvis du ikke er klar over det.

Det er også viktig å merke seg at `Calendar` tar hensyn til forskjellige tidssoner, så det kan være lurt å spesifisere hvilken tidssone du vil bruke når du oppretter en instans.

## Se også

- [Java Calendar Documentation](https://docs.oracle.com/javase/8/docs/api/java/util/Calendar.html)
- [Java Date Documentation](https://docs.oracle.com/javase/8/docs/api/java/util/Date.html)
- [Java SimpleDateFormat Documentation](https://docs.oracle.com/javase/8/docs/api/java/text/SimpleDateFormat.html)