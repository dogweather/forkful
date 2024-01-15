---
title:                "Oversette en streng til små bokstaver"
html_title:           "Java: Oversette en streng til små bokstaver"
simple_title:         "Oversette en streng til små bokstaver"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/java/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Hvorfor
Når du jobber med tekstbehandling i Java, kan det være nødvendig å konvertere en streng til små bokstaver. Dette gjør det enklere å sammenligne og behandle tekst på en mer konsistent måte. 

## Slik gjør du det
For å konvertere en streng til små bokstaver, kan du bruke metoden `toLowerCase()` fra `String` klassen. Denne metoden tar ikke inn noen argumenter og returnerer en ny streng med kun små bokstaver.

```
Java String eksempelStreng = "HELLO WORLD";

System.out.println(eksempelStreng.toLowerCase());
// output: hello world
```

## Dypdykk
Når vi bruker `toLowerCase()` metoden, blir alle store bokstaver i en streng konvertert til små. Dette inkluderer også bokstaver fra andre språk enn engelsk. Det betyr at teksten vil bli konvertert til små bokstaver uavhengig av språket det opprinnelig var skrevet i. 

Det er også viktig å være oppmerksom på at `toLowerCase()` ikke endrer originalstrengen. Den returnerer en kopi av strengen med små bokstaver. For å endre originalstrengen, må man tilordne den nye strengen til variabelen som inneholder den opprinnelige strengen.

## Se også 
- [Java String dokumentasjon](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html)
- [Working with Strings in Java](https://www.baeldung.com/java-strings)
- [Java String Methods](https://www.javatpoint.com/java-string-methods)