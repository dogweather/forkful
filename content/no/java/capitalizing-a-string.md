---
title:                "Capitalisering av en streng"
html_title:           "Java: Capitalisering av en streng"
simple_title:         "Capitalisering av en streng"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/java/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Hvorfor

Hvorfor bry seg om å konvertere en streng til store bokstaver? Vel, vær ærlig - vi har alle vært der. Skrivefeil i navn, glemt å bruke Caps Lock, eller kanskje du bare vil gjøre en beskjed mer lesbar. Uansett grunn, å kunne konvertere en streng til store bokstaver er en nyttig ferdighet for enhver Java-utvikler.

## Slik gjør du det

Det er flere måter å konvertere en streng til store bokstaver i Java, men her er to enkle eksempler som bruker innebygde metoder:

```Java
// Opprett en streng som skal konverteres
String tekst = "dette skal til store bokstaver";

// Bruk toUpperCase() metoden
String storeBokstaver = tekst.toUpperCase();
System.out.println(storeBokstaver); // "DETTE SKAL TIL STORE BOKSTAVER"

// Bruk toLocaleUpperCase() metoden med Locale argumentet
String storeBokstaverNorsk = tekst.toLocaleUpperCase(new Locale("no"));
System.out.println(storeBokstaverNorsk); // "DETTE SKAL TIL STORE BOKSTAVER"
```

Som du kan se, begge metodene gir samme resultat, men den andre metoden lar deg angi et språk ved hjelp av Locale-objektet.

## Dypdykk

Hvis du vil dykke dypere inn i konvertering av strenger til store bokstaver, kan du bruke StringBuffer-klassen. Denne klassen inneholder en metode kalt toUpperCase(), som konverterer hver bokstav i en streng til store bokstaver. Her er et eksempel:

```Java
StringBuffer sb = new StringBuffer(tekst);
sb.setCharAt(0, Character.toUpperCase(sb.charAt(0)));
sb.setCharAt(6, Character.toUpperCase(sb.charAt(6)));

System.out.println(sb.toString()); // "Dette Skal til store bokstaver"
```

Denne metoden er nyttig hvis du bare vil konvertere enkelte bokstaver i en streng til store bokstaver.

## Se også

For å lære mer om Java-strenger og innebygde metoder, sjekk ut disse ressursene:

- [Java String klasse dokumentasjon](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html)
- [Java StringBuffer klasse dokumentasjon](https://docs.oracle.com/javase/8/docs/api/java/lang/StringBuffer.html)
- [Java Locale klasse dokumentasjon](https://docs.oracle.com/javase/8/docs/api/java/util/Locale.html)