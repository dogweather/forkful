---
title:                "Java: Søk og erstatt tekst"
simple_title:         "Søk og erstatt tekst"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/java/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Hvorfor

Å søke og erstatte tekst er en viktig del av programmering fordi det tillater oss å gjøre store endringer i koden vår på en effektiv måte. Ved å bruke søke- og erstatningsfunksjoner kan vi spare mye tid og unngå manuell feil når vi jobber med store mengder tekst. 

## Hvordan 

For å søke og erstatte tekst i Java, kan vi bruke innebygde metoder som `replace()` og `replaceAll()`. Her er et eksempel på hvordan vi kan erstatte en del av en streng med en annen:

```java
String originalTekst = "Hei, verden!";
String nyTekst = originalTekst.replace("Hei", "Hallo");

// output: Hallo, verden!
System.out.println(nyTekst); 
```

Her erstatter vi "Hei" med "Hallo" i strengen "Hei, verden!". Ved å bruke `replaceAll()` kan vi også bruke regulære uttrykk for å søke og erstatte tekst.
 
```java
String originalTekst = "Jeg elsker å kode i Java!";
String nyTekst = originalTekst.replaceAll("Java\\b", "Kotlin");

// output: Jeg elsker å kode i Kotlin!
System.out.println(nyTekst); 
```

I dette tilfellet erstatter vi alle forekomster av "Java" med "Kotlin". Vi bruker `\\b` for å sikre at vi bare erstatter hele ordet "Java" og ikke deler av andre ord som inneholder bokstavkombinasjonen.

## Dypdykk 

Å søke og erstatte tekst ved hjelp av regulære uttrykk kan være en svært kraftig måte å manipulere tekst på. Her er noen nyttige tips for å få mest mulig ut av dette verktøyet:

- bruk `\\b` for å matche hele ord og unngå å erstatte deler av andre ord
- bruk `+` for å matche en eller flere forekomster av en bokstav eller tall
- bruk `*` for å matche null eller flere forekomster av en bokstav eller tall
- bruk `^` for å matche starten av en streng
- bruk `$` for å matche slutten av en streng
- bruk `|` for å matche flere ord eller uttrykk samtidig
- bruk `\s` for å matche mellomrom og linjeskift
- bruk `\\d` for å matche tall

Et annet viktig poeng å merke seg er at `replaceAll()` og `replace()` returnerer en ny streng i stedet for å endre den originale strengen. Dette betyr at du må lagre den returnerte strengen i en variabel for å få tilgang til den endrede teksten.

## Se også 

- [Java regex tutorial](https://www.vogella.com/tutorials/JavaRegularExpressions/article.html)
- [Oracle docs - String class](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/lang/String.html)
- [W3Schools - Java String replace() method](https://www.w3schools.com/java/ref_string_replace.asp)