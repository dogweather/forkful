---
title:                "Java: Store bokstaver i en tekststreng"
simple_title:         "Store bokstaver i en tekststreng"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/java/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Hvorfor

Å kunne formatere tekst er en viktig del av programmering, spesielt når det kommer til å få riktig utseende på utskrifter og brukergrensesnitt. Å kunne kapitalisere en streng, eller gjøre den om til store bokstaver, er en vanlig oppgave som kan være nyttig i mange programmeringsoppgaver.

## Hvordan

For å kapitalisere en streng i Java, kan man bruke metoden .toUpperCase(). Her er et eksempel på hvordan dette kan se ut i kode:

```Java 
String navn = "ola nordmann";
System.out.println(navn.toUpperCase());
```

Dette vil gi følgende utskrift: "OLA NORDMANN". Metoden .toUpperCase() endrer altså strengen til å inneholde kun store bokstaver.

Det finnes også anledninger der man ønsker å bare kapitalisere første bokstav i en streng, og la resten forbli små bokstaver. Dette kan gjøres ved å bruke metoden .substring() og kombinere den med .toUpperCase(). Se eksempelet under:

```Java
String navn = "ola nordmann";
String kapitalisert = navn.substring(0, 1).toUpperCase() + navn.substring(1).toLowerCase();
System.out.println(kapitalisert);
```

Her vil utskriften bli: "Ola nordmann", med kun den første bokstaven kapitalisert og resten i små bokstaver.

## Dypdykk

Det er verdt å merke seg at .toUpperCase() og .toLowerCase() metodene tar hensyn til språkkontekst. Dette betyr at hvis man for eksempel har norske bokstaver i en streng, vil disse også bli kapitalisert eller gjort om til små bokstaver i henhold til norske regler.

Det finnes også andre metoder for å kapitalisere en streng i Java, som for eksempel StringUtils.capitalize() fra Apache Commons library. Denne metoden tar hensyn til flere språk og kulturer, og kan være nyttig å bruke i mer komplekse programmer.

## Se også

- [String documentation](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html)
- [Apache Commons StringUtils.capitalize()](https://commons.apache.org/proper/commons-lang/apidocs/org/apache/commons/lang3/StringUtils.html#capitalize-java.lang.String-)