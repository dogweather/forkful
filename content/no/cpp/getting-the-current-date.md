---
title:                "Å få gjeldende dato"
html_title:           "C++: Å få gjeldende dato"
simple_title:         "Å få gjeldende dato"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/cpp/getting-the-current-date.md"
---

{{< edit_this_page >}}

### Hva og Hvorfor?

Å få den nåværende datoen er en vanlig oppgave for programmerere i mange forskjellige applikasjoner. Dette gjøres for å gi brukerne nøyaktig og relevant informasjon, for eksempel i datobaserte kalkulatorer, kalendere, eller for å holde oversikt over hendelser og tidsfrister. 

### Hvordan:

```C++
#include <iostream>
#include <ctime>

int main() {
  // Hent tidspunktet for nåværende dato
  std::time_t now = std::time(0);
  
  // Konverter tidspunktet til streng og skriv ut 
  std::cout << "Nåværende dato og klokkeslett: " << std::ctime(&now);
  
  return 0;
}
```

Output:
```
Nåværende dato og klokkeslett: Thu Jan 23 14:31:00 2020
```

### Dykk dypere:

Denne funksjonen ble først introdusert i C-språket, men er nå også en del av C++. Det finnes også alternative måter å få den nåværende datoen på, for eksempel å hente den fra operativsystemet eller fra en tredjeparts bibliotek. Implementasjonen av denne funksjonen kan variere mellom forskjellige operativsystemer og kompilatorer.

### Se også:

For mer informasjon om å få den nåværende datoen i C++, se disse lenkene:

- [cppreference.com - std::time](https://en.cppreference.com/w/cpp/chrono/c/time)
- [stackoverflow.com - Current Date and Time in C++](https://stackoverflow.com/questions/1442116/how-to-get-the-current-date-and-time-in-c)