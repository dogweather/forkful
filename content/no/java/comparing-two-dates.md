---
title:    "Java: Sammenligning av to datoer"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/java/comparing-two-dates.md"
---

{{< edit_this_page >}}

# Hvorfor

Å sammenligne to datoer er en vanlig oppgave i programmering, spesielt når man jobber med å lage applikasjoner som involverer datoer og tid. Dette kan være for å sjekke om en hendelse skjedde før eller etter en annen, eller å sortere data etter dato. Uansett hva årsaken måtte være, er det viktig å ha gode ferdigheter i å sammenligne datoer for å kunne lage pålitelige og nøyaktige programmer.

# Hvordan

For å sammenligne to datoer i Java, kan man bruke klassen `LocalDate` fra `java.time` pakken. Her er et eksempel på hvordan du kan sammenligne to datoer og få ut en boolsk verdi som svar:

```Java
// Oppretter to lokale datoer
LocalDate dato1 = LocalDate.of(2021, 10, 15);
LocalDate dato2 = LocalDate.of(2021, 10, 18);

// Sammenligner datoene og lagrer resultatet i en boolsk variabel
boolean erDato1Tidligere = dato1.isBefore(dato2);

// Printer ut resultatet
System.out.println("Er dato1 tidligere enn dato2? " + erDato1Tidligere);
```

Dette kodeeksempelet vil gi følgende utskrift:

```
Er dato1 tidligere enn dato2? true
```

Det finnes også andre metoder som kan brukes til å sammenligne datoer, som for eksempel `isAfter()` og `isEqual()`. Disse vil returnere henholdsvis `true` hvis den første datoen er senere enn den andre, og `true` hvis de to datoene er like.

# Dypdykk

Når man sammenligner to datoer, er det viktig å være klar over at datohåndtering kan være komplisert. Noen av de vanligste feilene man kan gjøre er å ikke ta hensyn til tidssoner eller sommertid, eller å ikke håndtere skuddår riktig. Det anbefales derfor å alltid være nøye når man arbeider med datoer og å bruke pålitelige biblioteker som `java.time` for å unngå feil.

Man bør også være klar over at når man sammenligner datoer som også har klokkeslett, vil disse bli tatt i betraktning. For eksempel, hvis man sammenligner datoene 2021-10-18 09:00 og 2021-10-18 10:00, vil det bli returnert `true` for `isBefore()` og `true` for `isAfter()`.

Det er også viktig å merke seg at klassen `LocalDate` bare representerer en dato og ikke et spesifikt klokkeslett. For å sammenligne to datoer som også inkluderer klokkeslett, kan man bruke klassen `LocalDateTime`.

# Se også

- [Java Documentation - LocalDate](https://docs.oracle.com/javase/8/docs/api/java/time/LocalDate.html)
- [Oracle Tutorial - Comparing Dates and Times in Java](https://docs.oracle.com/javase/tutorial/datetime/iso/compare.html)
- [Baeldung - Comparing Dates in java.time](https://www.baeldung.com/java-compare-dates)