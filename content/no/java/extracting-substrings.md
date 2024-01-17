---
title:                "Ekstrahering av delstrenger"
html_title:           "Java: Ekstrahering av delstrenger"
simple_title:         "Ekstrahering av delstrenger"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/java/extracting-substrings.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Å trekke ut delstrenger er en prosess der man henter ut en del av en tekststreng. Dette er en vanlig oppgave for programmører når de behandler tekstlige data, som for eksempel å finne et bestemt mønster i en tekst eller å manipulere en tekststreng for å få ønsket resultat.

## Slik gjør du det:

```Java
// Eksempel på å trekke ut delstrenge fra en tekststreng
String tekst = "Dette er en tekst";
String delstreng = tekst.substring(5); // Starter på indeks 5 (e) og henter ut resten av strengen

System.out.println(delstreng); // Skriver ut "er en tekst"
```

## Dypdykk:

Det å trekke ut delstrenger har vært en del av programmering siden de tidlige dager. Det finnes flere måter å gjøre det på, som for eksempel ved å bruke indeksering eller regulære uttrykk. I Java finnes det også flere metoder for å trekke ut delstrenger, som for eksempel ```substring()``` som vi viste i eksempelet.

## Se også:

- [Oracle Java dokumentasjon om hvordan man bruker ```substring()```](https://docs.oracle.com/javase/10/docs/api/java/lang/String.html#substring)
- [Java Tutorials: Manipulating Characters in a String](https://docs.oracle.com/javase/tutorial/java/data/manipstrings.html)