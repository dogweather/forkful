---
title:    "Java: Beregning av en dato i fremtiden eller fortiden"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/java/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Hvorfor

Å kunne beregne en dato i fremtiden eller fortiden er en nyttig ferdighet som kan komme godt med i mange Java-programmer. For eksempel kan det være nødvendig å beregne leveringstiden for en bestilling, eller å finne ut når en betalingsfrist utløper. Å kunne beregne datoer i Java gjør det mulig å automatisere slike oppgaver og spare tid og ressurser.

## Hvordan

For å kunne beregne en dato i Java kan du bruke klassen `LocalDate` fra `java.time` pakken. Denne klassen lar deg manipulere datoer ved hjelp av enkle metoder. Her er et eksempel på hvordan du kan beregne datoen 7 dager fra i dag:

```Java
LocalDate today = LocalDate.now();
LocalDate futureDate = today.plusDays(7);
System.out.println("Datoen 7 dager fra i dag er: " + futureDate);
```

Dette vil gi følgende utskrift: `Datoen 7 dager fra i dag er: <Datoen 7 dager frem i tid>`

Du kan også bruke andre metoder som `plusMonths()` eller `minusYears()` for å beregne datoer i fremtiden eller fortiden basert på måneder og år.

## Dypdykk

Hvis du ønsker å forstå mer om hvordan datoer beregnes i Java, kan du se nærmere på klassen `LocalDate`. Denne klassen bruker en logisk metode kalt "flyttende dag" som gjør at datoer kan beregnes korrekt, selv når det er skuddår eller sommertid. Det er også viktig å være bevisst på hvilken tidssone du arbeider med når du bruker `LocalDate` klassen.

## Se også

- [Oracle Java API dokumentasjon for LocalDate](https://docs.oracle.com/javase/8/docs/api/java/time/LocalDate.html)
- [Tutorial om java.time pakken](https://www.baeldung.com/java-8-date-time-intro)
- [Artikkel om å beregne datoer i Java](https://www.techbeamers.com/java-8-date-time-api/)