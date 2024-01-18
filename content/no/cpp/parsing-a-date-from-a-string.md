---
title:                "Analyze en dato fra en streng."
html_title:           "C++: Analyze en dato fra en streng."
simple_title:         "Analyze en dato fra en streng."
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/cpp/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Å parse en dato fra en streng betyr å konvertere en streng som representerer en dato til et format som datamaskinen kan forstå. Dette er nyttig for programmerere for å kunne behandle datoer og gjøre beregninger basert på dem.

## Slik Gjør Du:

```C++
#include <iostream>
#include <sstream>
#include <ctime>

int main() {
  std::string date = "15/05/2021";
  
  // Oppretter en stringstream med strengen som mål
  std::stringstream ss(date);

  // Setter opp formatet til datoen vi vil parsere
  std::tm time = {};
  ss >> std::get_time(&time, "%d/%m/%Y");
  if (ss.fail()) {
    std::cout << "Datoen er ugyldig!" << std::endl;
  } else {
    // Konverterer datoen til en tidspunkt med std::mktime
    std::time_t t = std::mktime(&time);
    std::cout << "Datoen er konvertert til et tidspunkt: " << std::ctime(&t) << std::endl;
  }
}
```

Output:

```
Datoen er konvertert til et tidspunkt: Sat May 15 00:00:00 2021
```

## I Dybden

Parsing av datoer fra strenger har blitt en vanlig oppgave for programmerere som jobber med å behandle og analysere data. Det har blitt spesielt viktig med den økende bruken av datoer i databaser og andre datasystemer.

Alternativene for å parse datoer var begrenset til begynnelsen av 1900-tallet, da de fleste programmeringsspråk ikke hadde innebygde funksjoner for dette. Dette førte til mye manuell og tidkrevende kode. Men med utviklingen av nye språk og biblioteker har dette blitt mye enklere og standardisert.

Etter at datoen er konvertert til en tidspunkt-variabel, kan den benyttes til å gjøre beregninger og sammenligninger med andre datoer. Det finnes også ulike formater man kan konvertere datoen til, avhengig av hva som er nødvendig for programmet ditt.

## Se Også

- [cppreference - std::get_time](https://en.cppreference.com/w/cpp/io/manip/get_time)
- [cplusplus.com - std::mktime](https://www.cplusplus.com/reference/ctime/mktime/)
- [Stack Overflow - Parse Date from String in C++](https://stackoverflow.com/questions/42172401/parse-date-from-string-in-c/42173726#42173726)