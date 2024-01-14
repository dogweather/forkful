---
title:    "C: Å få gjeldende dato"
keywords: ["C"]
---

{{< edit_this_page >}}

## Hvorfor

Å få den nåværende datoen er en felles oppgave for mange programmerere, og det kan være nyttig for en rekke formål. Enten du lager en kalenderapplikasjon, logger når en fil ble opprettet, eller trenger å vise den nåværende datoen i et program, så er det viktig å forstå hvordan man kan få tak i dette i C-programmering.

## Hvordan

Først og fremst må vi inkludere `<time.h>` biblioteket i koden vår for å kunne bruke funksjoner relatert til tid og dato. Deretter kan vi bruke `time()` funksjonen til å få tak i den nåværende datoen i sekunder siden 1. januar 1970. For å konvertere dette til en lesbar dato, bruker vi `localtime()` funksjonen og lagrer resultatet i en `struct tm` variabel. Her er et eksempel på hvordan dette kan gjøres:

```C
#include <stdio.h>
#include <time.h>

int main() {
    // Få den nåværende dato og lagre den i en struct tm variabel
    time_t now = time(0);
    struct tm *current_time = localtime(&now);
    
    // Skriv ut den nåværende datoen i et leselig format
    printf("Dagens dato er: %d-%d-%d\n", current_time->tm_mday, current_time->tm_mon + 1, current_time->tm_year + 1900);
    
    return 0;
}
```

Dette vil produsere følgende output:

```
Dagens dato er: 25-11-2021
```

Å bruke `tm_mon` og `tm_year` variablene kan virke litt forvirrende, men det er fordi de representerer måned og år som et null-indeksert tall. Derfor må vi legge til 1 i `tm_mon` og 1900 i `tm_year` for å få den faktiske måneden og året.

## Deep Dive

Nå som vi har en grunnleggende forståelse av hvordan vi kan få tak i den nåværende datoen i C, kan vi se nærmere på noen av funksjonene som er tilgjengelige i `<time.h>` biblioteket. Her er noen av de vanligste funksjonene og hva de gjør:

- `time_t time(time_t *time_ptr)`: Returnerer den nåværende datoen og tiden i sekunder siden 1. januar 1970. Hvis en peker er gitt som argument, lagrer den også datoen og tiden der.
- `struct tm *localtime(const time_t *time_ptr)`: Konverterer datoen og tiden gitt fra `time_t` til en lesbar `struct tm` variabel basert på den lokale tidssonen.
- `struct tm *gmtime(const time_t *time_ptr)`: Konverterer datoen og tiden gitt fra `time_t` til en lesbar `struct tm` variabel basert på UTC (koordinert universaltid).
- `char *asctime(const struct tm *time_ptr)`: Konverterer en `struct tm` variabel til en tekststreng i et leselig format.
- `time_t mktime(struct tm *time_ptr)`: Konverterer en `struct tm` variabel til en `time_t` variabel.

Det finnes også andre nyttige funksjoner, så det kan være lurt å sjekke ut dokumentasjonen for å se alle mulighetene.

## Se Også

- [Dokumentasjon for `<time.h>` biblioteket på cplusplus.com](http://www.cplusplus.com/reference/ctime/)
- [En guide om hvordan man bruker tid og dato-funksjoner i C](https://www.tutorialspoint.com/c_standard_library/c_function_localtime.htm)
- [En video som viser hvordan man bruker tid og dato-funksjoner i C](https://www.youtube.com/watch?v=_ixg4jKcOUw)