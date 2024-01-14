---
title:    "C++: Sammenligning av to datoer"
keywords: ["C++"]
---

{{< edit_this_page >}}

## Hvorfor

Å sammenligne to datoer kan være en nyttig ferdighet å ha når du jobber med C++-programmering. Dette kan hjelpe deg med å ordne og sortere datoer, og gjøre det enklere å håndtere ulike tidsrelaterte oppgaver. Det kan også være nyttig å sammenligne datoer når du for eksempel skal analysere data over en bestemt periode. I denne bloggposten vil vi gå gjennom hvordan du kan sammenligne to datoer ved hjelp av C++.

## Hvordan

For å sammenligne to datoer i C++, må du først definere de to datoene som variabler. Dette kan gjøres ved å bruke datatypen `time_t`, som representerer tiden som antall sekunder siden 1. januar 1970. Deretter kan du bruke funksjonen `difftime()` til å sammenligne de to datoene.

```C++
#include <iostream>
#include <ctime>

int main()
{
    // Definerer to datoer som variabler
    time_t dato1 = 1500000000; // 12. juli 2017
    time_t dato2 = 1600000000; // 13. september 2020
    
    // Beregner differansen mellom de to datoene
    double differanse = difftime(dato2, dato1);

    // Skriver ut resultatet
    std::cout << "Differansen mellom de to datoene er " << differanse << " sekunder." << std::endl;

    return 0;
}
```

Eksempeloutput:

```
Differansen mellom de to datoene er 100000000 sekunder.
```

I dette tilfellet er differansen mellom datoene gitt i sekunder. Hvis du ønsker å få differansen i for eksempel dager eller måneder, kan du bruke enkle matematiske beregninger for å konvertere tiden.

## Deep Dive

C++ har også flere innebygde funksjoner som kan hjelpe deg med å sammenligne datoer på en mer nøyaktig måte. For eksempel kan du bruke funksjonen `localtime()` til å få en mer lesbar representasjon av dato og tid basert på variabelen `time_t`.

```C++
#include <iostream>
#include <ctime>

int main()
{
    // Definerer en dato som variabel
    time_t dato = time(0); // Nåværende dato og tid
    
    // Konverterer datoen til en lesbar form
    struct tm *dagens_dato = localtime(&dato);

    // Skriver ut resultatet
    std::cout << "Dagens dato er " << dagens_dato->tm_mday << "/" << (dagens_dato->tm_mon + 1) << "/" << (dagens_dato->tm_year + 1900) << std::endl;

    return 0;
}
```

Eksempeloutput:

```
Dagens dato er 1/10/2021
```

I tillegg til dette har C++ også andre funksjoner som kan hjelpe deg med å håndtere og sammenligne datoer og tider, som for eksempel `mktime()` og `strftime()`. Det kan være nyttig å utforske disse funksjonene nærmere for å lære mer om hvordan du kan håndtere datoer i C++.

## Se også

- [C++ time.h referanse](https://www.cplusplus.com/reference/ctime/)
- [C++ datetime bibliotek](https://github.com/HowardHinnant/date)
- [En guide til C++ dato og tid biblioteker](https://cpptruths.blogspot.com/2014/11/a-guide-to-c-datetime-libraries.html)