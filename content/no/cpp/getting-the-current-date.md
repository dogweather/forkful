---
title:    "C++: Få den nåværende datoen"
keywords: ["C++"]
---

{{< edit_this_page >}}

# Hvorfor

Å få den nåværende datoen er en vanlig oppgave i programmering. Det kan være nyttig for å vise aktuell informasjon eller for å beregne tidsintervaller. Uansett formål, er det viktig å kunne få riktig dato i koden din.

# Hvordan å få den nåværende datoen i C++

Det finnes flere måter å få den nåværende datoen i C++ på, men den mest populære metoden er å bruke standard bibliotekfunksjoner. For å få den nåværende datoen, må du inkludere "ctime" biblioteket øverst i koden din.

```C++
#include <ctime>
```
 
Deretter må du bruke funksjonen "time" fra biblioteket for å få den nåværende datoen som en struct av typen "tm". Denne structen inneholder informasjon om dager, måneder, år og klokkeslett.

```C++
time_t now = time(0);
tm *current = localtime(&now);
```

For å hente ut den nåværende datoen, kan du bruke følgende funksjoner:

- current->tm_mday: for å få dag nummeret
- current->tm_mon + 1: for å få måneden (månedene er representert som tall fra 0 til 11, derfor legger vi til 1 for å få riktig måned)
- current->tm_year + 1900: for å få året (året er representert som antall år siden 1900)
- current->tm_hour: for å få timene
- current->tm_min: for å få minuttene
- current->tm_sec: for å få sekundene

La oss se på et eksempel for å få den nåværende datoen og klokkeslettet:

```C++
// Får den nåværende datoen og klokkeslettet
#include <iostream>
#include <ctime>

using namespace std;

int main() {
    // Får den nåværende datoen og klokkeslettet
    time_t now = time(0);
    tm *current = localtime(&now);

    // Printer ut datoen og klokkeslettet i ønsket format
    cout << "Dato: " << current->tm_mday << "/" << current->tm_mon + 1 << "/" << current->tm_year + 1900 << endl;
    cout << "Klokkeslett: " << current->tm_hour << ":" << current->tm_min << ":" << current->tm_sec << endl;

    return 0;
}
```

Output:
```
Dato: 29/10/2021
Klokkeslett: 22:35:00
```

# Dypdykk

Dato og klokkeslett kan også hentes ut ved hjelp av andre metoder som for eksempel "chrono" biblioteket eller ved å bruke systemspesifikke funksjoner. Det er viktig å merke seg at den nåværende datoen og klokkeslettet hentet med disse metodene kan variere avhengig av tidssone og systeminnstillinger.

# Se også

- [C++ Date and Time functions](https://www.cplusplus.com/reference/ctime/)
- [C++ Chrono Library](https://www.cplusplus.com/reference/chrono/)
- [Systemspesifikke funksjoner for dato og tid i C++](https://en.cppreference.com/w/cpp/chrono)