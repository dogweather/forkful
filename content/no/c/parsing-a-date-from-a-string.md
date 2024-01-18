---
title:                "Analysering av dato fra en streng"
html_title:           "C: Analysering av dato fra en streng"
simple_title:         "Analysering av dato fra en streng"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Parsing av en dato fra en streng betyr å konvertere en tekstrepresentasjon av en dato (for eksempel "12. mai 2021") til en datatypen som er brukt i programmering (for eksempel en "Date" variabel). Dette gjøres for å kunne manipulere datoer på en mer effektiv måte i programmet.

## Hvordan å:
Her er et eksempel på hvordan man kan parse en dato fra en streng i C:

```C
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

int main() {
    // Definer string med dato
    char dato[] = "12.05.2021";

    // Konverter stringen til en "tm" struct ved hjelp av "strptime" funksjonen
    struct tm t;
    strptime(dato, "%d.%m.%Y", &t);

    // Konverter "tm" structen til en "time_t" variabel ved hjelp av "mktime" funksjonen
    time_t tid = mktime(&t);

    // Skriv ut den parsede datoen
    printf("Datoen er: %s", ctime(&tid));
    
    return 0;
}
```
Dette vil gi følgende output:
```
Datoen er: Wed May 12 00:00:00 2021
```
Her brukes funksjoner som "strptime" og "mktime" fra <time.h> biblioteket for å konvertere datoen til en "time_t" variabel som kan brukes i programmet.

## Dypdykk:
Parsing av datoer fra strenger har blitt enklere med introduksjonen av standard bibliotekfunksjoner som "strptime" og "mktime". Før dette måtte programmører ofte skrive egne funksjoner for å konvertere datoer. Det finnes også alternative måter å representere og manipulere datoer på, som for eksempel bruk av "struct tm" og "asctime" funksjonen.

## Se også:
- [time.h - C++ Reference](https://www.cplusplus.com/reference/ctime/)
- [Date and Time Programming in C](https://www.mitchr.me/SS/exampleCode/complex/dateFunctions/)# Begreper og Hvorfor?
Analyse av dato fra en tekst streng handler om å endre en tekstrepresentasjon av en dato (f.eks. "22 desember 2022") til en data som brukes innen programmering (f.eks. en "Dato" verdi). Dette er nyttig for at datoer skal kunne brukes effektivt innad i programfilene. 

## Slik gjør du:
Her er et eksempel på hvordan du kan analysere en dato fra en tekst streng i C:

```C
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

int main() {
    // Angi datostrengen
    char dato[] = "22.12.2022";

    // Endre datostrengen til en "tm" struct via "strptime" funksjonen
    struct tm t;
    strptime(dato, "%d.%m.%Y", &t);

    // Endre "tm" structen til en "time_t" verdi, ved hjelp av "mktime" funksjonen
    time_t tid = mktime(&t);

    // Skriver ut analysert dato
    printf("Dato er: %s", ctime(&tid));
    
    return 0;
}
```

Koden over vil skrive ut følgende resultat:
```
Dato is: Wed Dec 22 00:00:00 2022
```

Vi bruker funksjoner som "strptime" og "mktime" fra biblioteket <time.h> for å analysere datoen og konvertere den til en "time_t" verdi som vi kan bruke i programmet vårt.

## Utforske dypere:
Det har blitt enklere å analysere datoer ved hjelp av funksjoner som "strptime" og "mktime" fra standard C biblioteket. Før måtte programmerere ofte lage sine egne funksjoner for å analysere og konvertere datoer. Det finnes også alternative måter å representere og manipulere datoer på, som for eksempel ved å bruke "struct tm" og "asctime" funksjonen.

## Se også:
- [time.h - C++ Reference](https://www.cplusplus.com/reference/ctime/)
- [Date and Time Programming in C](https://www.mitchr.me/SS/exampleCode/complex/dateFunctions/)