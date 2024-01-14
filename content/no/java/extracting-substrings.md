---
title:                "Java: Utvinning av delstrenger"
simple_title:         "Utvinning av delstrenger"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/java/extracting-substrings.md"
---

{{< edit_this_page >}}

# Hvorfor
Ofte i programmering trenger man å jobbe med tekststrenger eller ord. Dette kan være alt fra å finne bestemte ord til å manipulere en hel tekststreng. Å kunne ekstrahere substringer fra en tekststreng er en viktig og nyttig ferdighet for enhver Java-programmerer. Det lar deg finne og bruke kun den delen av teksten du trenger, noe som kan spare deg for mye tid og arbeid.

# Hvordan man gjør det
Ekstrahering av substringer i Java er enkelt å implementere ved hjelp av den innebygde metoden `substring()`. Denne metoden gjør det mulig å hente ut en del av en tekststreng basert på startindeks og sluttindeks. La oss se på et eksempel:

```Java
String ord = "Hei alle sammen!";
String del = ord.substring(4, 7);
System.out.println("Del av ordet: " + del);
```

Koden over vil gi følgende utskrift:

```
Del av ordet: all
```

I dette eksempelet bruker vi `substring()`-metoden til å hente ut delen av ordet fra indeks 4 til 7, som tilsvarer "all" i teksten. Det er viktig å huske at Java bruker null-indeksering, noe som betyr at den første bokstaven i teksten har indeks 0.

Det er også mulig å bruke `substring()`-metoden uten å spesifisere en sluttindeks. I så fall kommer den til å hente ut alle tegn fra startindeksen og frem til slutten av teksten. La oss se på et annet eksempel:

```Java
String setning = "Jeg elsker å programmere i Java!";
String del = setning.substring(12);
System.out.println("Del av setningen: " + del);
```

Dette vil gi oss følgende utskrift:

```
Del av setningen: programmere i Java!
```

# Dypdykk
Det finnes flere måter å bruke `substring()`-metoden på. I tillegg til å angi start- og sluttindeks, kan du også bruke `substring()` til å finne indexen til en bestemt streng inne i en tekststreng. Du kan gjøre dette ved å bruke `indexOf()`-metoden sammen med `substring()`. Et eksempel på dette kan se slik ut:

```Java
String tekst = "Dette er en tekststreng.";
int indeks = tekst.indexOf("tekst");
String del = tekst.substring(indeks);
System.out.println("Del av teksten: " + del);
```

Dette vil gi oss følgende utskrift:

```
Del av teksten: tekststreng.
```

Som du kan se bruker vi `indexOf()`-metoden til å finne indexen til ordet "tekst" i teksten. Deretter bruker vi denne indexen som startindeks til `substring()`-metoden for å hente ut resten av teksten etter dette ordet.

# Se også
- [String API i Java](https://docs.oracle.com/javase/9/docs/api/java/lang/String.html)
- [Hvordan jobbe med tekst i Java](https://www.baeldung.com/java-text-tutorial)
- [Java substring-metoden](https://www.w3schools.com/java/ref_string_substring.asp)