---
title:                "C++: Konvertere en dato til en streng"
simple_title:         "Konvertere en dato til en streng"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/cpp/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Hvorfor

Å konvertere en dato til en streng kan være nyttig for å vise datoer i et mer leselig format for brukere, eller for å lagre datoer som en del av en filnavn eller databaseoppføring.

## Slik gjør du det

Konvertering av en dato til en streng kan gjøres enkelt ved å bruke et innebygd bibliotek i C++ kalt "stringstream". Følgende er et eksempel på hvordan du kan bruke dette biblioteket for å konvertere en dato til en streng:

```C++
#include <iostream>
#include <sstream> // inkluder stringstream biblioteket
#include <ctime> // inkluder ctime biblioteket for å få tilgang til dato- og tidsfunksjoner

int main() {
    time_t now = time(0); // hent nåværende dato og tid

    // konverter til en streng ved å bruke stringstream
    stringstream ss;
    ss << put_time(localtime(&now), "%d.%m.%Y"); // bruker ønsket format her

    // lagre den konverterte datoen som en streng i en variabel
    string date = ss.str();

    // skriv ut den konverterte datoen
    cout << "Dagens dato: " << date << endl;

    return 0;
}
```

Dette vil gi følgende utdata:

```
Dagens dato: 26.04.2021
```

## Dykk dypere

Det å konvertere en dato til en streng kan bli mer komplisert dersom man ønsker å bruke et spesifikt datoformat eller lokal tidssone. I eksempelet over bruker vi "%d.%m.%Y" for å få datoen i formatet dag.måned.år, men man kan endre dette til ønsket format.

I tillegg kan man også bruke forskjellige funksjoner fra ctime-biblioteket for å få tilgang til andre aspekter ved datoen, for eksempel dag i uken eller klokkeslett.

## Se også

- [Completing conversions in C++](https://www.learncpp.com/cpp-tutorial/completing-conversions-in-cpp/)
- [StringStream class reference](https://www.cplusplus.com/reference/sstream/stringstream/)
- [Date and time functions in C++](https://www.cplusplus.com/reference/ctime/)