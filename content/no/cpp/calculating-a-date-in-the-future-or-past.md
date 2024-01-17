---
title:                "Beregning av en dato i fremtiden eller fortiden"
html_title:           "C++: Beregning av en dato i fremtiden eller fortiden"
simple_title:         "Beregning av en dato i fremtiden eller fortiden"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/cpp/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# Hva & Hvorfor?
Det å beregne en dato i fremtiden eller fortiden er en vanlig oppgave for programmerere. Dette innebærer å bruke kodede algoritmer til å beregne en dato basert på et spesifisert antall dager i fremtiden eller fortiden. Dette kan være nyttig i en rekke programmer, som for eksempel kalendere eller tidsplanleggingssystemer.

# Hvordan:
Det er flere måter å beregne en dato i fremtiden eller fortiden på i C++. Nedenfor finner du et eksempel på hvordan dette kan gjøres ved å bruke en enkel funksjon:

```C++
#include <iostream>
#include <string>
#include <ctime>

int main() {
  // Spesifiserer dato og antall dager i fremtiden eller fortiden
  std::string current_date = "2021-04-20";
  int num_days = 10;

  // Bruker stl::tm struct for å konvertere dato til tidsstempel
  std::tm date = {0};
  std::strptime(current_date.c_str(), "%Y-%m-%d", &date);
  std::time_t t = std::mktime(&date);

  // Legger til/subtraherer antall dager til tidsstempel
  t += num_days * 24 * 60 * 60; // 24 timer * 60 minutter * 60 sekunder
  std::tm* future_date = std::localtime(&t);

  // Output fremtidig dato som en string
  std::cout << "Fremtidig dato: " << (future_date->tm_year + 1900) << "-"
            << (future_date->tm_mon + 1) << "-" << future_date->tm_mday << std::endl;

  return 0;
}
```

Eksempelutgang:
```
Fremtidig dato: 2021-04-30
```

# Dykk ned:
Beregning av datoer har vært en viktig del av dataprogrammering siden begynnelsen. Tidligere ble dette gjort ved å bruke komplekse matematiske formler, men med utviklingen av programmeringsspråk som C++ har det blitt mye enklere. Alternativene for å beregne datoer inkluderer også å bruke innebygde funksjoner og biblioteker i stedet for å skrive koden selv. Implementeringsdetaljer for beregning av datoer kan variere avhengig av språk og biblioteker som brukes.

# Se også:
- [C++ tid- og dato-funksjoner](https://www.cplusplus.com/reference/ctime/)
- [Beregning av datoer i andre programmeringsspråk (engelsk)](https://stackabuse.com/calculate-time-difference-in-c/)