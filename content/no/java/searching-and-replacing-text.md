---
title:                "Java: Å søke og erstatte tekst"
programming_language: "Java"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/java/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Hvorfor 

Enten du er en erfaren Java-programmerer eller en nybegynner, er det viktig å vite hvordan man effektivt søker og erstatter tekst i dine programmer. Dette kan hjelpe deg med å effektivisere koden din og spare tid i utviklingsprosessen.

## Hvordan 

For å søke og erstatte tekst i Java-programmer, kan du bruke metoden "replace" fra klassen String. Denne metoden tar to parametere, det gamle ordet du vil erstatte og det nye ordet du vil erstatte det gamle med.

```Java
String tekst = "Hei, dette er en tekst som skal erstattes.";
String nyTekst = tekst.replace("erstattes", "endres");
System.out.println(nyTekst);
```

Output: Hei, dette er en tekst som skal endres.

Det er også mulig å bruke regulære uttrykk for å søke og erstatte tekst i Java-programmer. Dette gir mer fleksibilitet og nøyaktighet i søket. Du kan bruke klassen Pattern fra Java.util-pakken og metoden replaceAll fra klassen String til å søke og erstatte tekst ved hjelp av regulære uttrykk.

```Java
String tekst = "Dette er en tekst som inneholder tall 123";
String nyTekst = tekst.replaceAll("[0-9]", "");
System.out.println(nyTekst);
```

Output: Dette er en tekst som inneholder tall 

## Dypdykk 

Det er viktig å være oppmerksom på at metoden "replace" erstatter alle forekomster av det gamle ordet, mens metoden "replaceAll" erstatter alle forekomster av det gamle ordet som passer til regulært uttrykk. Dette kan være nyttig hvis du ønsker å endre deler av en tekst som opptrer flere ganger i koden din.

Det er også mulig å bruke metoden "replaceFirst" for å erstatte kun den første forekomsten av det gamle ordet. Denne metoden tar også to parametere, det gamle ordet og det nye ordet.

```Java
String tekst = "Hei, dette skal erstattes, hei!";
String nyTekst = tekst.replaceFirst("hei", "hallo");
System.out.println(nyTekst);
```

Output: Hallo, dette skal erstattes, hei!

## Se også 

- [Java String documentation](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html)
- [RegEx in Java](https://www.vogella.com/tutorials/JavaRegularExpressions/article.html)
- [String Replace, ReplaceAll and ReplaceFirst in Java](https://www.baeldung.com/java-string-replace-replaceall-replacefirst)