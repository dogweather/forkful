---
title:    "Java: Sammenføying av strenger"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/java/concatenating-strings.md"
---

{{< edit_this_page >}}

## Hvorfor

Å kombinere strenger er en viktig del av Java programmering. Med denne enkle handlingen kan du legge sammen flere strenger og danne en ny. Dette er nyttig for å lage tekstutdata eller for å bygge komplekse uttrykk som krever å sette sammen flere deler av informasjon.

## Hvordan gjøre det

For å sette sammen strenger i Java, bruker du "+" operatøren. Du trenger bare å plassere den mellom de to strengene du ønsker å kombinere. La oss se på et enkelt eksempel:

```Java
String navn = "Maria";
String hilsen = "Hei, mitt navn er " + navn;
System.out.println(hilsen);
```

I dette eksempelet vil programmet skrive ut "Hei, mitt navn er Maria". Du kan også kombinere flere strenger ved å bruke flere "+" operatører, for eksempel:

```Java
String fornavn = "John";
String etternavn = "Doe";
String fulltNavn = fornavn + " " + etternavn;
System.out.println(fulltNavn);
```

Dette vil skrive ut "John Doe".

## Dypere dykk

En viktig ting å huske når du kombinerer strenger i Java er at verdier av andre typer, som for eksempel tall eller boolean, vil bli automatisk konvertert til string før de blir satt sammen. For eksempel:

```Java
int nummer = 25;
String tallTekst = "Jeg er nummer " + nummer;
System.out.println(tallTekst);
```

Dette vil skrive ut "Jeg er nummer 25". Også, hvis du prøver å kombinere en streng med "null", vil det resultere i at strengen "null" blir satt sammen. Det er derfor viktig å passe på å håndtere spesielle tilfeller og sørge for at strenger blir kombinert på en måte som gir mening.

## Se også

- [Offisiell Java Strings dokumentasjon](https://docs.oracle.com/javase/tutorial/java/data/strings.html)
- [Tutorialspoint Java Strings tutorial](https://www.tutorialspoint.com/java/java_strings.htm)
- [En guide til konkatenering i Java](https://www.geeksforgeeks.org/java-string-concatenation/)