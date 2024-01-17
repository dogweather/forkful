---
title:                "Konvertere en dato til en streng"
html_title:           "Java: Konvertere en dato til en streng"
simple_title:         "Konvertere en dato til en streng"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/java/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

# Hva og hvorfor?
Konvertering av en dato til en streng er en vanlig operasjon i Java programmering. Dette er når datoen, som er lagret som et tall, blir gjort om til tekst for enklere lesbarhet for brukeren. Dette gjøres ofte for å vise datoen til en bruker eller for å lagre datoen som en tekst i en database.

# Hvordan:
```Java
// Opprett en datoobjekt med datoen 12. november 2020
Date dato = new Date(2020, 11, 12);

// Konverter datoen til en streng ved å bruke SimpleDateFormat
SimpleDateFormat formatter = new SimpleDateFormat("dd/MM/yyyy");
String datoSomStreng = formatter.format(dato);
System.out.print(datoSomStreng); // Output: 12/11/2020
```

# Dypdykk:
Konvertering av datoer til strenger har vært en del av Java siden dens tidlige versjoner. Første utgave av Java, versjon 1.0, hadde allerede klassen SimpleDateFormat som kunne konvertere datoer til strenger og omvendt. Det finnes også alternative måter å konvertere datoer til strenger på, for eksempel ved hjelp av LocalDate og DateTimeFormatter klasser som ble introdusert i Java 8. Implementeringen av konvertering av datoer til strenger kan variere avhengig av hvilken versjon av Java du bruker, men grunnleggende konsepter er de samme.

# Se også:
- Java Date og Calendar klasser: https://docs.oracle.com/javase/8/docs/api/java/util/Date.html
- LocalDate og DateTimeFormatter klasser: https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html