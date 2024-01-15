---
title:                "Slette tegn som samsvarer med et mønster"
html_title:           "Java: Slette tegn som samsvarer med et mønster"
simple_title:         "Slette tegn som samsvarer med et mønster"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/java/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Hvorfor
Enten man skal lage et program eller modifisere eksisterende kode, kan det hende man trenger å slette bestemte tegn som matcher et spesifikt mønster. Det kan være for å fjerne uønskede tegn eller for å utføre en annen type manipulasjon av teksten. Uansett årsak, er det nyttig å kunne slette tegn som matcher et bestemt mønster i Java.

## Hvordan
For å slette tegn som matcher et mønster i Java, kan man bruke metoden `replaceAll()` fra `String`-klassen. Denne metoden tar inn to parameter: det første parameteret er mønsteret man skal matche, og det andre parameteret er hva man vil bytte de matchende tegnene med. Her er et eksempel på bruk av `replaceAll()`:

```Java
String tekst = "Dette er en tekst.";
String nyTekst = tekst.replaceAll("e", "a");
System.out.println(nyTekst);
```

Dette vil gi følgende utskrift: "Datta ar an tastk." Som du kan se, har alle forekomster av "e" i teksten blitt erstattet med "a".

Man kan også bruke regulære uttrykk i mønsteret for å slette flere tegn på én gang. For eksempel kan man bruke `[0-9]` for å matche alle tall, og `\\W` for å matche alle ikke-alfabetiske tegn. Her er et eksempel som sletter alle tall og ikke-alfabetiske tegn fra en tekst:

```Java
String tekst = "Dette er en tekst123.";
String nyTekst = tekst.replaceAll("[0-9\\W]", "");
System.out.println(nyTekst);
```

Dette vil gi følgende utskrift: "Detteerentekst". Alle tall og ikke-alfabetiske tegn er blitt slettet fra teksten.

## Dypdykk
Når man bruker `replaceAll()` for å slette tegn som matcher et mønster, må man være oppmerksom på at den tar inn et regulært uttrykk som mønster. Det betyr at man må bruke escape-sekvenser som `\\` for å matche spesialtegn som `+`, `*` og `.`.

Det finnes flere metoder i Java som kan hjelpe med å slette tegn som matcher et mønster, som for eksempel `replace()`, `replaceFirst()` og `replaceLast()`. Disse kan også være nyttige å lære om for mer avansert tegnbehandling.

## Se også
- [Java String replaceAll() method](https://www.javatpoint.com/java-string-replaceall)
- [Regular Expressions in Java](https://www.baeldung.com/java-regex)
- [Java String documentation](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html)