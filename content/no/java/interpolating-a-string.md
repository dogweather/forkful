---
title:                "Interpolering av en streng"
html_title:           "Java: Interpolering av en streng"
simple_title:         "Interpolering av en streng"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/java/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Hva og Hvorfor?
Interpolering av strenger er en metode som lar oss sette variabler inn i en tekststreng på en enkel måte. Dette gjør det lettere å bygge dynamiske og tilpassede meldinger eller utskrifter. Programmere bruker vanligvis interpolering for å lage mer dynamiske og leservennlige strenger i koden sin.

## Hvordan:
For å interpolere en tekststreng i Java, må du bruke merkelappen "%s" der du vil sette inn en variabel. Deretter, nederst i koden din, legger du til "System.out.printf("Dette er en %s tekst", variabel);". Når programmet kjører, vil variabelens verdi bli satt inn i tekststrengen og utskriften vil være "Dette er en [variabel] tekst".

```java
String navn = "Nora";
System.out.printf("Hei, mitt navn er %s. Velkommen!", navn);
```

Utskrift: Hei, mitt navn er Nora. Velkommen!

## Dykk dypere:
Interpolering ble først introdusert i programmeringsspråket C og har siden blitt implementert i mange andre språk, inkludert Java. Alternativer til å bruke interpolering kan være å bruke konkatenering eller String.format-metoden. Når du bruker interpolering, må du være forsiktig med å sikre at du bruker riktig formateringskode (%s, %d, %f osv.) for å matche variabelens datatyper.

## Se også:
For mer informasjon om interpolering av strenger i Java, se dokumentasjonen: [Java - String Formatting](https://docs.oracle.com/javase/tutorial/java/data/numberformat.html)