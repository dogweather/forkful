---
title:                "Sammenslåing av strenger"
html_title:           "Java: Sammenslåing av strenger"
simple_title:         "Sammenslåing av strenger"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/java/concatenating-strings.md"
---

{{< edit_this_page >}}

## Hvorfor

Har du noen gang ønsket å kombinere flere tekststrenger til én? Det kan være nyttig for å lage komplekse setninger eller skrive ut variabelverdier sammen med støttetekst. I Java, kan du gjøre dette ved å bruke stringkonkatenasjon (string concatenation). Les videre for å lære hvordan du kan gjøre dette!

## Hvordan

For å konkatenere flere strenger, kan du bruke "+" -operatøren. Dette vil kombinere de to strengene og returnere en ny streng som inneholder begge. La oss se på et eksempel:

```Java
String navn = "Jan";
String etternavn = "Jansen";
String fulltNavn = navn + " " + etternavn;

System.out.println(fulltNavn);
```

Denne koden vil skrive ut "Jan Jansen" til konsollen. Merk at vi har brukt " " (mellomrom) i mellom navnet og etternavnet for å få et mellomrom mellom dem i den kombinerte strengen.

Du kan også kombinere strenger med tall. For eksempel:

```Java
int alder = 25;
String beskrivelse = "Jeg er " + alder + " år gammel.";
System.out.println(beskrivelse);
```

Denne koden vil skrive ut "Jeg er 25 år gammel." til konsollen.

## Dype dypere

Når du bruker "+" -operatøren for å kombinere strenger, må du være oppmerksom på datatypekonvertering. Hvis du prøver å kombinere en streng og et tall, vil Java automatisk konvertere tallet til en streng før de kombineres. Dette er grunnen til at vi kunne bruke "+" -operatøren i eksemplene våre ovenfor uten å konvertere alderen til en streng først.

I tillegg, hvis du bruker "+" -operatøren flere ganger på rad, vil Java utføre concatenation i sekvens. For eksempel:

```Java
String tekst1 = "Hei ";
String tekst2 = "på ";
String tekst3 = "deg!";
String kombinert = tekst1 + tekst2 + tekst3;

System.out.println(kombinert);
```

Denne koden vil skrive ut "Hei på deg!" til konsollen.

## Se også

- [Java String concatenation tutorial](https://www.tutorialspoint.com/java/java_string_concatenation.htm)
- [Official Java documentation on String concatenation](https://docs.oracle.com/javase/tutorial/java/data/converting.html)