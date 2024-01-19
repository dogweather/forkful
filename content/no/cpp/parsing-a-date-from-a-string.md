---
title:                "Tolke en dato fra en streng"
html_title:           "Bash: Tolke en dato fra en streng"
simple_title:         "Tolke en dato fra en streng"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/cpp/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Hva og Hvorfor?

Å parse en dato fra en streng er prosessen med å konvertere en tekstuell representasjon av datoen til et datoto objekt som en datamaskin kan forstå og manipulere. Dette gjøres for å håndtere datoer på en mer strukturert og pålitelig måte.

## Hvordan:

Her er et eksempel på hvordan du gjør parsing av en dato fra en streng i C++:
```C++
#include <iostream>
#include <sstream>
#include <iomanip>
#include <chrono>

int main() {
    std::istringstream date_stream("2023-11-24");
    std::tm date = {};
    date_stream >> std::get_time(&date, "%Y-%m-%d");

    if (date_stream.fail()) {
        std::cout << "Failed to parse date!\n";
    } else {
        std::cout << std::put_time(&date, "%Y-%m-%d") << "\n";
    }
    return 0;
}
```
Utskriften av programmet vil være:
```
2023-11-24
```
Hvis den ikke klarer å parse datoen, vil du få et "Failed to parse date!"-melding.

## Fordypning

Historisk sett har programmerere laget egne funksjoner for å parse datoer. Men med fremveksten av moderne programmeringsspråk, som C++, er det standardiserte funksjoner og biblioteker tilgjengelig for å utføre denne oppgaven. Alternativt til 'get_time'-funksjonen, kan du bruke 'strptime'-funksjonen i C.

Her er noen detaljer angående implementasjonen: 'get_time' funksjonen brukes til å lese tegn fra en innstrømsbuffer, parse dem i henhold til formatet gitt (i dette tilfellet "%Y-%m-%d"), og lagre de produserte verdier i de medlemmene av 'tm'-strukturen. Hvis parsingen mislyktes, vil 'failbit'-flagget være satt til 'date_stream'.

## Se Også

For en dypere forståelse av hvordan å parse datoer I C++, kan du sjekke ut følgende kilder:

- [cppreference](https://en.cppreference.com/w/cpp/io/manip/get_time)
- [cplusplus](http://www.cplusplus.com/reference/ctime/strptime/?kw=strptime)
- [StackOverflow](https://stackoverflow.com/questions/14136833/stdget-time-vs-strptime) discussion on get_time versus strptime.