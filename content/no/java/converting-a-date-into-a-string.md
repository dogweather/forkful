---
title:                "Omgjøring av dato til en streng"
html_title:           "Java: Omgjøring av dato til en streng"
simple_title:         "Omgjøring av dato til en streng"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/java/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Hvorfor

Det kan være nyttig å konvertere en dato til en streng i Java for å kunne formatere og vise datoen på en mer lesbar måte for brukere, eller for å lagre datoen som en tekst i en database.

## Hvordan gjøre det
```Java
// Opprett en Date-variabel
Date dato = new Date();

// Definer ønsket format for datoen
SimpleDateFormat sdf = new SimpleDateFormat("dd/MM/yyyy");

// Konverter datoen til en streng
String strDato = sdf.format(dato);

// Skriv ut strengen
System.out.println(strDato); // Output: 29/10/2021
```

## Dypdykk
For å konvertere en dato til en streng, kan man bruke klassen SimpleDateFormat i Java. Denne klassen lar deg definere et ønsket format for datoen, som hentet fra eksempelet over (dd/MM/yyyy). 
Det finnes flere formateringsalternativer, som for eksempel å legge til tidsinformasjon eller endre rekkefølgen på dag, måned og år. For mer informasjon om hvordan du kan formatere datoen etter dine behov, kan du sjekke ut dokumentasjonen til SimpleDateFormat-klassen.

## Se også
- [Java Date to String](https://www.javatpoint.com/date-to-string-in-java)
- [SimpleDateFormat Documentation](https://docs.oracle.com/javase/8/docs/api/java/text/SimpleDateFormat.html)