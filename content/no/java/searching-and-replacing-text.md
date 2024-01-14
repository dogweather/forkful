---
title:    "Java: Søking og erstattin"
keywords: ["Java"]
---

{{< edit_this_page >}}

## Hvorfor

Som en programmerer, har du mest sannsynlig opplevd behovet for å endre eller erstatte en bestemt tekst i koden din. Kanskje du har skrevet en lang kode og oppdaget en skrivefeil, eller kanskje du vil endre navnet på en variabel gjennom hele koden din. Uavhengig av årsaken, vil å søke og erstatte tekst være en nyttig ferdighet å ha.

## Hvordan gjøre det

For å søke og erstatte tekst i Java, kan du bruke metoden `replace()` som er tilgjengelig for strenger. Denne metoden lar deg finne en bestemt tekst i en streng og erstatte den med en annen tekst. Her er et enkelt eksempel som viser hvordan du kan bruke `replace()`-metoden:

```Java
String original = "Dette er en tekst for å vise søking og erstatting.";
String nyTekst = original.replace("tekst", "kode");

System.out.println(original);
System.out.println(nyTekst);
```

I dette eksemplet vil teksten "Dette er en tekst for å vise søking og erstatting." bli skrevet ut to ganger, men den andre utskriften vil ha ordet "tekst" erstattet med "kode". Dette er bare et enkelt eksempel, men det viser hvordan du kan bruke `replace()`-metoden.

Du kan også bruke regulære uttrykk for å gjøre mer avansert søking og erstatting. For å gjøre dette, bruker du metoden `replaceAll()` i stedet for `replace()`. Her er et eksempel på hvordan dette kan se ut:

```Java
String original = "Jeg liker å programere i Java, Java er mitt favorittspråk.";
String nyTekst = original.replaceAll("Java", "Python");

System.out.println(original);
System.out.println(nyTekst);
```

I dette tilfellet vil begge forekomstene av "Java" i den originale strengen bli erstattet med "Python".

## Dypdykk

Ä nå man tar i bruk søking og erstatting, er det viktig å være oppmerksom på at det ikke alltid vil være nøyaktig det du forventer. For eksempel vil `replace()`-metoden bare erstatte den første forekomsten av teksten den finner. Hvis du vil erstatte alle forekomster, må du bruke `replaceAll()`-metoden.

Du bør også være forsiktig med bruk av regulære uttrykk, da de kan være komplekse og gi uventede resultater hvis du ikke vet hva du gjør. Det er viktig å teste uttrykkene dine og sørge for å forstå hvordan de fungerer før du bruker dem i koden din.

## Se også

- Java String API: https://docs.oracle.com/javase/8/docs/api/java/lang/String.html
- Regulære uttrykk i Java: https://docs.oracle.com/javase/tutorial/essential/regex/