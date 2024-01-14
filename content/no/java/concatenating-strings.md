---
title:    "Java: Sammenstilling av strenger."
keywords: ["Java"]
---

{{< edit_this_page >}}

## Hvorfor

Å legge sammen strenger er en viktig del av programmering i Java. Det lar deg kombinere forskjellige tekster og variabler for å lage en mer kompleks og dynamisk tekststreng. Dette er spesielt nyttig når du for eksempel lager utskrifter eller viser informasjon til brukeren.

## Hvordan

For å legge sammen strenger i Java, bruker du "+" operatøren. La oss si at du har to variabler, "navn" og "alder":

```Java
String navn = "Ola";
int alder = 25;
```

For å kombinere disse to variablene i en tekststreng, kan du gjøre følgende:

```Java
String beskrivelse = "Hei, mitt navn er " + navn + " og jeg er " + alder + " år gammel.";
System.out.println(beskrivelse);
```

Dette vil gi følgende utskrift:

```
Hei, mitt navn er Ola og jeg er 25 år gammel.
```

Som du kan se, legges variablene og tekstene sammen for å lage en komplett setning.

## Dypdykk

Når du bruker "+" operatøren for å legge sammen strenger, blir det laget en ny streng hver gang. Dette betyr at du kan kombinere så mange tekster og variabler du ønsker. For eksempel:

```Java
String fornavn = "Per";
String etternavn = "Hansen";
int fødselsår = 1990;
String navn = fornavn + " " + etternavn;
String beskrivelse = "Du er født i " + fødselsår + " og heter " + navn;
```

Dette vil gi utskriften:

```
Du er født i 1990 og heter Per Hansen.
```

En annen viktig ting å huske på når du legger sammen strenger er at du må sørge for at variablene er av riktig datatype. Hvis ikke kan det oppstå feil i koden din.

## Se også

- [Java String concatenation](https://www.javatpoint.com/String-Concatenation-in-java)
- [Official Java String documentation](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html)