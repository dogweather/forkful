---
title:                "Java: Konvertere en streng til små bokstaver"
simple_title:         "Konvertere en streng til små bokstaver"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/java/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Hvorfor
Noen ganger trenger vi å konvertere en streng til små bokstaver i Java-programmering. Dette er nyttig for blant annet å sammenligne to strenger eller for å følge standard konvensjoner for tekstbehandling.

## Hvordan gjøre det
Det finnes flere måter å konvertere en streng til små bokstaver i Java, men den enkleste metoden er å bruke "toLowerCase()" metoden. Her er et eksempel:

```java
String tekst = "Dette er EN ekSeMpEl";

String konvertetTekst = tekst.toLowerCase();

System.out.println(konvertetTekst);
```

Dette vil gi følgende output:

```bash
dette er en eksempel
```

## Dypdykk
Når vi bruker "toLowerCase()" metoden i Java, vil alle bokstaver i strengen bli konvertert til små bokstaver. Dette inkluderer også spesialtegn, tall og mellomrom. Det finnes også en annen metode som heter "toUpperCase()" som vil konvertere alle bokstaver til store. Det kan være lurt å utforske disse metodene for å se hvordan de fungerer.

En annen ting å merke seg er at Java bruker "UTF-16" karakterkoding, noe som betyr at konvertering til små bokstaver vil variere basert på språket som brukes. For eksempel vil en bokstav som "Å" konverteres til "å" i norsk, men til "a" i engelsk. Det kan være viktig å være klar over dette når du jobber med flere språk.

## Se også
- [Java String API documentation](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html)
- [Java String.toLowerCase() method](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html#toLowerCase--)
- [Java String.toUpperCase() method](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html#toUpperCase--)