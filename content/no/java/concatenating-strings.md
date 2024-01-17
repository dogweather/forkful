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

Velkommen til ditt raske og informative introduksjonskurs om hvordan å konkatenering av strenger i Java! Hvis du er en Java-utvikler eller ønsker å bli en, er dette et essensielt konsept å forstå.

## Hva & Hvorfor?

Så hva betyr egentlig å konkatenering av strenger? Det er ganske enkelt prosessen med å kombinere to eller flere strenger til en enkelt streng. Dette gjøres ved hjelp av en pluss (+) operator i Java. Programmerere gjør dette ofte for å lage en ny og lengre streng som inneholder informasjonen fra de forskjellige strengene.

## Hvordan:

La oss se på et enkelt eksempel på hvordan du konkatenerer strenger i Java:

```Java
String fornavn = "John";
String etternavn = "Smith";
String fulltNavn = fornavn + " " + etternavn;
System.out.println(fulltNavn);
```

I dette eksempelet, bruker vi en kombinasjon av variabler og tekst for å opprette en ny streng, som blir tilordnet til variabelen fulltNavn. Koden vil da skrive ut fulltNavn, som vil være "John Smith".

Du kan også konkatenerere flere strenger sammen som vist i dette eksempelet:

```Java
String navn = "John" + " " + "Smith";
System.out.println(navn);
```

Dette vil også skrive ut "John Smith".

## Dypdykk:

Konkatenering av strenger ble introdusert i Java 1.0, og har vært en viktig del av språket siden da. Alternativene til å bruke pluss operator inkluderer bruk av StringBuilders og StringBuffers, som er mer effektive når du håndterer store mengder data.

## Se også:

For mer informasjon om konkatenering av strenger i Java, sjekk ut disse kildene:

- https://docs.oracle.com/javase/tutorial/java/data/strings.html
- https://www.geeksforgeeks.org/java-string-clone-method/
- https://www.tutorialspoint.com/java/java_strings.htm

Lykke til med å bruke det du har lært til å gjøre komplekse strenger i Java på null komma niks!