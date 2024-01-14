---
title:    "Java: Å beregne en dato i fremtiden eller fortiden"
keywords: ["Java"]
---

{{< edit_this_page >}}

## Hvorfor
Å beregne en dato i fremtiden eller fortiden kan være nyttig for å planlegge og organisere hendelser eller for å spore viktige datoer. Det kan også være nyttig for å foreta timing av oppgaver eller for å analysere datahistorikk.

## Hvordan
Det finnes flere måter å beregne datoer i Java på, men en enkel metode er å bruke "Calendar" klassen. Først må vi opprette et "Calendar" objekt og deretter bruke metoder som "add()" eller "set()" for å justere datoen etter våre behov. Her er et eksempel på hvordan man kan beregne datoen for 30 dager frem i tid:

```Java
// Oppretter et "Calendar" objekt for nåværende dato
Calendar cal = Calendar.getInstance();

// Legger til 30 dager til nåværende dato
cal.add(Calendar.DAY_OF_MONTH, 30);

// Henter ut datoen som et "Date" objekt
Date futureDate = cal.getTime();

// Skriver ut datoen i ønsket format
System.out.println("Datoen 30 dager frem i tid er: " + futureDate);
```

Dette eksempelet vil gi følgende output: 
*Datoen 30 dager frem i tid er: Sat, Aug 08 2020*

## Dypdykk
Noen av de nyttige metodene for å beregne datoer med "Calendar" klassen inkluderer:
- "add()" for å legge til en viss verdi til en spesifikk del av datoen (f.eks. 30 dager til måneden).
- "set()" for å sette en spesifikk verdi til en del av datoen (f.eks. å sette datoen til en bestemt dag).
- "getTime()" for å hente ut datoen som et "Date" objekt.
- "get()" for å hente ut en spesifikk del av datoen (f.eks. måneden eller året).

Det er også verdt å merke seg at "Calendar" klassen følger det gregorianske kalendersystemet, og at det finnes ulike metoder for å håndtere tidssoner og lokale tider. Det kan være lurt å lese gjennom dokumentasjonen for å få et bedre innblikk i alle mulighetene denne klassen har å tilby.

## Se også
- [Java Calendar Dokumentasjon](https://docs.oracle.com/javase/8/docs/api/java/util/Calendar.html)
- [Java Date and Time Tutorials](https://www.baeldung.com/java-8-date-time-intro)
- [Java Date Format Eksempler](https://www.javatpoint.com/java-date-format)