---
title:                "C++: Hente dagens dato"
programming_language: "C++"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/cpp/getting-the-current-date.md"
---

{{< edit_this_page >}}

# Hvorfor

Å få dagens dato er en vanlig oppgave i programmering. Dette kan være nyttig for å holde oversikt over tidsangivelser, logge hendelser, eller generelt vise brukeren den nåværende datoen. Å kunne få dagens dato i et program kan være en nyttig ferdighet å ha, uansett hvilken type program du lager.

# Hvordan

Å få dagens dato i C++ er en enkel prosess. Først må du inkludere <ctime> biblioteket, som inneholder funksjoner for å håndtere datoer og tider. Deretter kan du bruke funksjonen `time()` til å få antall sekunder siden 1. januar 1970. Dette konverteres så til en `tm` struktur ved hjelp av funksjonen `localtime()`.

```C++
#include <ctime>

// Få antall sekunder siden 1. januar 1970
time_t now = time(0);

// Konverter til `tm` struktur
tm *ltm = localtime(&now);
```

Ved å bruke `ltm` strukturen kan du nå få tilgang til forskjellige elementer som dag, måned og år. For eksempel, for å få dagens dato, kan du bruke `ltm->tm_mday`, som vil returnere verdien av dagens dato som en heltallsverdi.

```C++
// Få dagens dato
int dag = ltm->tm_mday;

// Få månedens nummer (januar = 0, desember = 11)
int måned = ltm->tm_mon + 1;

// Få året siden 1900
int år = ltm->tm_year + 1900;
```

Du kan også formatere datoen ved å bruke `strftime()` funksjonen og en `char`-array. For eksempel kan følgende kode brukes for å få datoen i formatet "DD.MM.YYYY".

```C++
char dato[11];

// Bruk `strftime()` funksjonen for å formatere datoen
strftime(dato, 11, "%d.%m.%Y", ltm);
```

### Sample Output

Lar oss si at vi kjører koden på datoen 22. april 2021. Da vil verdiene vi får for dag, måned og år være 22, 4 og 2021.

Hvis vi bruker `strftime()` funksjonen, vil `dato` arrayen inneholde "22.04.2021" som resultat.

# Deep Dive

Hvis du vil gå enda dypere inn i hvordan du håndterer datoer i C++, kan du se nærmere på `ctime` biblioteket og dets funksjoner. Det finnes også andre biblioteker som `chrono` som gir mer avanserte muligheter for å håndtere datoer og tider. Du kan også se nærmere på formateringsalternativer for `strftime()` funksjonen for å tilpasse utseendet på datoen din.

# Se Også

- [ctime biblioteket i C++](https://www.cplusplus.com/reference/ctime/)
- [chrono biblioteket i C++](https://www.cplusplus.com/reference/chrono/)
- [strftime() funksjonen i C++](https://www.cplusplus.com/reference/ctime/strftime/)