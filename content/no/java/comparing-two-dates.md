---
title:                "Sammenligner to datoer"
html_title:           "Java: Sammenligner to datoer"
simple_title:         "Sammenligner to datoer"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/java/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å sammenligne to datoer er en vanlig oppgave for programmerere som trenger å håndtere datoer og tidsstempel i koden sin. Det lar dem programmere logikk basert på datoer og ha kontroll over hvordan de behandler tidsbaserte data.

## Hvordan:
Java gir en rekke innebygde metoder for å sammenligne datoer og tidsstempel. Her er et eksempel på hvordan du kan gjøre det:

```Java
// Opprett to LocalDate-objekter
LocalDate dato1 = LocalDate.of(2021, 8, 1);
LocalDate dato2 = LocalDate.of(2021, 8, 15);

// Bruk compareTo-metoden for å sammenligne dato1 med dato2
int resultat = dato1.compareTo(dato2);

// Output: dato1 kommer før dato2
System.out.println("Resultatet av å sammenligne dato1 og dato2: " + resultat);
```

## Dypdykk:
Å sammenligne datoer har vært en viktig del av programmering helt siden konseptet med datamaskiner ble introdusert. I Java er det flere alternativer for å sammenligne datoer og tidsstempel, som for eksempel å bruke metoder som `equals()` og `isAfter()`. Implementasjonen av disse metodene kan variere basert på den nøyaktigheten og presisjonen som kreves for applikasjonen.

## Se også:
- [Java Date and Time API](https://docs.oracle.com/javase/8/docs/api/java/time/package-frame.html)
- [Java 8 Date and Time Tutorials](https://www.oracle.com/java/technologies/javase/javase8-archive-downloads.html)