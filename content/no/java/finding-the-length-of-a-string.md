---
title:                "Å finne lengden på en streng"
html_title:           "Java: Å finne lengden på en streng"
simple_title:         "Å finne lengden på en streng"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/java/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

# Hva & Hvorfor?
Å finne lengden av en streng er en vanlig oppgave i programmering, spesielt når vi jobber med tekstbehandling. Med lengden av en streng mener vi antall tegn i den gitte strengen. Dette kan være nyttig for å begrense brukerens inndata, eller for å sjekke om en streng er tom eller ikke.

# Hvordan:
Java har en innebygd metode som heter `length()`, som kan brukes til å finne lengden av en streng. Denne metoden returnerer antall tegn i strengen som er angitt i parentes. Her er et eksempel:

```Java
String navn = "Ingrid";
System.out.println(navn.length());
```

Dette vil skrive ut 6, siden det er 6 tegn i ordet "Ingrid".

# Dypdykk:
Funksjonen for å finne lengden av en streng er en del av Java-klassen `String`. Denne metoden ble introdusert i Java 1.0 og er fortsatt en av de mest brukte metodene i dag. Det finnes også andre måter å finne lengden av en streng på, som for eksempel å bruke en løkke for å telle antall tegn. Det er også viktig å merke seg at `length()`-metoden ikke fungerer fult ut for multibyte tegn i Unicode.

# Se også:
- Oracle sitt offisielle Java-dokumentasjon for [`length()`-metoden](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html#length--) (på engelsk)
- Stack Overflow-diskusjon om [hvilken metode som er best for å finne lengden på en streng](https://stackoverflow.com/questions/9080475/shortest-way-to-get-the-length-of-a-string) (på engelsk)