---
title:                "Stor bokstaver i en streng"
html_title:           "Java: Stor bokstaver i en streng"
simple_title:         "Stor bokstaver i en streng"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/java/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Hva og hvorfor?
I Java, er å capitalisere en streng å endre den første bokstaven til en stor bokstav og resten av bokstavene til små bokstaver. Dette gjøres ofte for å gjøre strenger mer leselig eller for å følge god programmeringspraksis.

## Hvordan:
Det er flere måter å capitalisere en streng på i Java. En vanlig metode er å bruke metoden "toUpperCase" og "toLowerCase" på karakterene i strengen, som vist i eksempelet nedenfor:

```Java
String str = "hallo verden";
System.out.println(str.toUpperCase()); // output: HALLO VERDEN
System.out.println(str.toLowerCase()); // output: hallo verden
```

En annen måte å gjøre dette på er å bruke metoden "substring" for å få tak i den første bokstaven og metoden "replaceFirst" for å endre den til stor bokstav, som vist nedenfor:

```Java
String str = "hallo verden";
String firstLetter = str.substring(0,1); // hentes ut første bokstav
String newStr = str.replaceFirst(firstLetter, firstLetter.toUpperCase());
System.out.println(newStr); // output: Hallo verden
```

## Dypdykk:
Capitalizing av strenger har vært en del av programmering siden tidligere språk som COBOL og FORTRAN. I dag er det fortsatt en viktig del av god programmeringspraksis for å gjøre koden mer leselig og for å følge konvensjonene i et språk. I tillegg til Java, finnes det også innebygde funksjoner for å capitalisere strenger i mange andre programmeringsspråk.

## Se også:
- [String API Dokumentasjon](https://docs.oracle.com/javase/10/docs/api/java/lang/String.html)
- [Capitalizing en String i Java - GeeksforGeeks](https://www.geeksforgeeks.org/capitalizing-first-letter-of-each-word-in-java/)