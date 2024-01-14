---
title:                "Java: Beregning av datoer i fremtiden eller fortiden"
programming_language: "Java"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/java/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Hvorfor 
Å beregne en dato i fremtiden eller fortiden kan være nyttig for å planlegge fremtidige aktiviteter eller for å spore tidligere hendelser. Ved å forstå hvordan man programmerer dette, kan man også få en bedre forståelse av datofunksjoner i Java og hvordan de kan brukes i ulike situasjoner.

## Hvordan
For å beregne en dato i fremtiden eller fortiden i Java, kan man bruke klassen Calendar og dens metoder. Her er et eksempel på hvordan dette kan gjøres:

```Java
// Opprett en instans av Calendar
Calendar cal = Calendar.getInstance();

// Sett ønsket dato
cal.set(2020, Calendar.SEPTEMBER, 15);

// Legg til 10 dager til datoen
cal.add(Calendar.DAY_OF_MONTH, 10);

// Få utskrift av datoen i ønsket format
System.out.println("Dato i fremtiden: " + cal.getTime());
```

Dette vil gi følgende output:

```
Dato i fremtiden: Mon Sep 25 00:00:00 PDT 2020
```

For å beregne en dato i fortiden, kan man bruke metoden `add` med et negativt tall, for eksempel `-10` for å trekke fra 10 dager.

## Dypdykk
Ved å bruke klassen Calendar kan man beregne datoer i fremtiden eller fortiden basert på ulike enheter som dager, uker, måneder eller år. Man kan også bruke forskjellige måter å representere datoer på, for eksempel å bruke en `int` for å representere måneder i stedet for å bruke begrepet `Calendar.DECEMBER`.

Det er viktig å merke seg at Java har en annen måte å håndtere datoer på enn enkelte andre programmeringsspråk, som for eksempel å telle måneder fra 0 i stedet for fra 1. Dette kan føre til forvirring når man jobber med datoer i Java.

For å få en dypere forståelse av datofunksjonene i Java og hvordan man kan bruke dem i ulike situasjoner, kan det være nyttig å lese mer om klassen Calendar og dens metoder.

## Se også
- [Offisiell dokumentasjon for Calendar i Java](https://docs.oracle.com/javase/8/docs/api/index.html?java/util/Calendar.html)
- [Tutorial om å beregne datoer i Java](https://www.baeldung.com/java-date-time-operations)
- [Tips for håndtering av datoer i Java](https://www.journaldev.com/10765/java-date-time-tips)