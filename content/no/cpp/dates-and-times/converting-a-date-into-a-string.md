---
title:                "Konvertere en dato til en streng"
aliases:
- /no/cpp/converting-a-date-into-a-string/
date:                  2024-01-20T17:36:29.708905-07:00
model:                 gpt-4-1106-preview
simple_title:         "Konvertere en dato til en streng"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/cpp/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Konvertering av en dato til en streng innebærer å endre datotypen fra ‘date’ (dato) til ‘string’ (tekststreng) for visning eller lagring. Dette gjøres for å kunne presentere datoer på et leservennlig format eller for å lagre i tekstbaserte systemer.

## Hvordan:
```C++
#include <iostream>
#include <iomanip>
#include <sstream>

int main() {
    std::tm t = {}; // Oppretter en tm-struktur for å holde datoen.
    t.tm_year = 123; // År siden 1900, så 2023 ville være 123.
    t.tm_mon = 3; // Måned siden januar, så april ville være 3.
    t.tm_mday = 7; // Dato i måneden, her bruker vi 7. april.
    
    std::ostringstream oss; // Oppretter en output string stream.
    oss << std::put_time(&t, "%Y-%m-%d"); // Formaterer datoen som "YYYY-MM-DD".
    
    std::string dateStr = oss.str(); // Konverterer strømmen til en string.
    std::cout << dateStr << std::endl; // Skriver ut den konverterte datostringen.
    
    return 0;
}
```
Sample output:
```
2023-04-07
```

## Dybdeplukking
Konvertering av datoer til tekststrenger er en gammel praksis som strekker seg tilbake til starten av programmering. I C++ ble dette tradisjonelt håndtert av C-bibliotekfunksjoner som `strftime`, men med introduksjonen av `std::put_time` i C++11, fikk vi en type-sikker og strømlinjeformet metode for å formatere datoer og tider.

Alternativer inkluderer bruk av tredjepartsbiblioteker som Boost.Date_Time eller Howard Hinnant's date library, som gir enda flere funksjoner for kompleks tidsmanipulasjon og parsing.

Når det gjelder implementasjonsdetaljer, bruker `std::put_time` en `tm` struktur for å representere tiden. Dette er en del av `<ctime>` biblioteket og gir en bro mellom C-tidfunksjoner og C++ iostream-funksjonalitet.

## Se Også
- C++ referanse for `std::put_time`: https://en.cppreference.com/w/cpp/io/manip/put_time
- Howard Hinnant's date library: https://github.com/HowardHinnant/date
- Boost Date_Time dokumentasjon: https://www.boost.org/doc/libs/release/libs/date_time/
