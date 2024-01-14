---
title:                "Java: Utvinning av delstrenger"
programming_language: "Java"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/java/extracting-substrings.md"
---

{{< edit_this_page >}}

## Hvorfor

I dagens digitale verden er det en konstant strøm av tekst som blir produsert og delt med andre. Noen ganger kan du ønske å kun arbeide med en del av denne teksten, i stedet for å håndtere hele tekstdokumentet. Det er her utvinning av substrings kommer inn i bildet. Ved å ekstrahere deler av teksten, kan du enkelt manipulere og behandle den på en mer effektiv måte. I denne bloggposten vil vi utforske hvordan du kan utvinne substrings i Java-programmering.

## Hvordan

For å utvinne substrings i Java, må du først kjenne lengden på teksten og hvilken del av teksten du ønsker å utvinne. Du kan gjøre dette ved hjelp av `length()` metoden og `substring()` metoden.

```Java
String tekst = "Hei, dette er en substring.";
int lengde = tekst.length();
String del = tekst.substring(4, lengde);
System.out.println(del);
```

Dette vil skrive ut: "dette er en substring." Det første argumentet i `substring()` metoden er startindeksen, mens det andre argumentet er sluttindeksen. Merk at sluttindeksen ikke er inkludert i den utvunnede substringen.

Hvis du ønsker å utvinne fra et bestemt punkt til slutten av teksten, trenger du bare å angi startindeksen og utelate sluttindeksen.

```Java
String tekst = "Vi elsker Java-programmering!";
String del = tekst.substring(12);
System.out.println(del);
```

Dette vil skrive ut: "Java-programmering!" Du kan også bruke negative indekser for å utvinne fra slutten av teksten.

```Java
String tekst = "Programmering er gøy!";
int lengde = tekst.length();
String del = tekst.substring(0, lengde-1);
System.out.println(del);
```

Dette vil skrive ut: "Programmering er gøy" siden sluttindeksen er utelatt og dermed blir den siste bokstaven utelatt.

## Dypdykk

I tillegg til å utvinne substrings, kan du også bruke metoden `indexOf()` for å finne hvor en bestemt tekst starter i en streng.

```Java
String tekst = "Java er et fantastisk programmeringsspråk";
int indeks = tekst.indexOf("fantastisk");
System.out.println(indeks);
```

Dette vil skrive ut: 12, siden ordet "fantastisk" begynner på indeks nummer 12.

Det er også viktig å merke seg at substrings i Java representeres ved bruk av `String`-klasse. Dette betyr at de er uforanderlige og kan ikke endres etter at de er opprettet. Hvis du ønsker å manipulere en substring, må du opprette en ny `String` som inneholder den ønskede endringen.

## Se også

For mer informasjon om utvinning av substrings og andre tekstmanipulasjonsmetoder i Java, kan du sjekke ut disse ressursene:

- [Java - Manipulating Strings (engelsk)](https://docs.oracle.com/javase/tutorial/java/data/manipstrings.html)
- [Java.util.String - Oracle Docs (engelsk)](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html)
- [W3Schools - Java String Methods (norsk)](https://www.w3schools.com/java/java_ref_string.asp)