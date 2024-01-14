---
title:    "Java: Konvertering av en dato til en streng"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/java/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Hvorfor

Å konvertere en dato til en streng er en vanlig oppgave i Java-programmering. Datoer er ofte representert som objekter i Java, men i visse tilfeller kan det være nyttig å konvertere dem til strenger for å lettere håndtere og vise dem. I denne bloggposten vil vi utforske hvorfor og hvordan man kan gjøre dette.

## Hvordan Konvertere en Dato til en Streng i Java

Konvertering av en dato til en streng i Java kan gjøres på flere måter, avhengig av hvilke behov du har. En enkel måte er å bruke `SimpleDateFormat`-klassen, som er en del av Java API. Her er et eksempel på hvordan du kan bruke det:

```java
// Opprett en formatter med ønsket strengformat
SimpleDateFormat formatter = new SimpleDateFormat("dd.MM.yyyy");

// Konverter en dato til en streng
Date date = new Date();
String strDate = formatter.format(date);

// Skriv ut resultatet
System.out.println(strDate); // Output: 05.11.2020
```

Du kan også tilpasse strengformatet ved å legge til flere variabler, som for eksempel tidspunkt og tidsone.

```java
// Opprett en formatter med ønsket strengformat
SimpleDateFormat formatter = new SimpleDateFormat("dd/MM/yyyy 'kl.' HH:mm z");

// Konverter en dato til en streng
Date date = new Date();
String strDate = formatter.format(date);

// Skriv ut resultatet
System.out.println(strDate); // Output: 05/11/2020 kl. 13:45 CET
```

## Deep Dive

Datoer er ofte representert som millisekunder siden 1. januar 1970 kalt "epoken" i Java. Dette er en standard måte å lagre datoer på og gjør det enkelt å beregne datoer og tidsintervaller. Når du konverterer en dato til en streng, brukes denne verdien til å generere en lesbar dato i henhold til det spesifiserte strengformatet.

En annen viktig ting å merke seg er at Java ikke er lokalisert, noe som betyr at standardstrengen for datoer vil være på engelsk, uansett hvilket språk du bruker. Hvis du ønsker å vise datoer på et annet språk, må du bruke `Locale`-klassen for å angi ønsket språk og språkkode.

## Se Også

- [Oracle dokumentasjon for SimpleDateFormat](https://docs.oracle.com/javase/8/docs/api/java/text/SimpleDateFormat.html)
- [Java Date og Calendar tutorial](https://www.baeldung.com/java-date-calendar)
- [Java String format tutorial](https://www.baeldung.com/java-string-format)