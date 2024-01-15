---
title:                "Oversette en dato til en streng."
html_title:           "C++: Oversette en dato til en streng."
simple_title:         "Oversette en dato til en streng."
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/cpp/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Hvorfor

Å konvertere en dato til en streng kan være nødvendig når man ønsker å presentere datoen på en spesifikk måte, for eksempel i et brukergrensesnitt eller i en rapport. Det kan også være nyttig når man skal lagre datoen i et bestemt format i en database.

## Hvordan

```C++
#include <iostream>
#include <string>
#include <iomanip>
#include <ctime>

using namespace std;

int main() {
  // Lag en dato variabel og sett den til dagens dato
  time_t now = time(0);
  tm *ltm = localtime(&now);
  string input_date = asctime(ltm);
  
  // Konverter datoen til en streng i det ønskede formatet
  string output_date = input_date.substr(8, 2) + "." + input_date.substr(4, 3) + "." + input_date.substr(20, 4);
  
  // Skriv ut den konverterte datoen
  cout << "Dagens dato: " << output_date << endl;
  
  return 0;
}
```

```
Output:
Dagens dato: 09.Jul.2021
```

## Deep Dive

Når man konverterer en dato til en streng, er det viktig å sørge for at formatet på datoen er riktig. For eksempel, i kodeeksempelet ovenfor, bruker vi funksjonen `asctime()` for å konvertere datoen til en streng, men dette resulterer i at måneden og året blir skilt med et enkelt mellomrom. Hvis dette ikke er ønskelig, kan man bruke andre funksjoner som `put_time()` eller `strftime()` for å formatere datoen på en mer nøyaktig måte.

For å unngå feil og sikre at datoen blir konvertert på riktig måte, kan man også sjekke at datoen er gyldig før man konverterer den til en streng. Dette kan gjøres ved å bruke funksjoner som `mktime()` og `localtime()` for å sjekke at alle delene av datoen er innenfor gyldige områder.

## Se også

- [http://www.cplusplus.com/reference/ctime/asctime/](http://www.cplusplus.com/reference/ctime/asctime/)
- [http://www.cplusplus.com/reference/ctime/put_time/](http://www.cplusplus.com/reference/ctime/put_time/)
- [http://www.cplusplus.com/reference/ctime/strftime/](http://www.cplusplus.com/reference/ctime/strftime/)