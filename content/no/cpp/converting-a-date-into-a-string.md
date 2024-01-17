---
title:                "Konvertere en dato til en streng"
html_title:           "C++: Konvertere en dato til en streng"
simple_title:         "Konvertere en dato til en streng"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/cpp/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

# Hva & Hvorfor?
 
Å konvertere en dato til en streng er en vanlig oppgave i programmering. Det betyr å transformere en dato, for eksempel 20. januar 2021, til en streng som kan lagres, vises og behandles av dataprogrammer. Dette gjøres for å gjøre det enklere for programmerere å håndtere datoer og for å gjøre det lettere å vise dem for brukere.

# Hvordan:

```C++
#include <iostream>
#include <string>
#include <ctime>
using namespace std;

int main()
{
    // Opprett en variabel for dagens dato
    time_t now = time(0);

    // Konverter datoen til en streng
    string date = ctime(&now);

    // Skriv ut datoen
    cout << "Dagens dato er: " << date << endl;

    // Konverter datoen til ISO-format
    char iso[80];
    strftime(iso, sizeof(iso), "%F", localtime(&now));

    // Skriv ut datoen i ISO-format
    cout << "Dagens dato i ISO-format er: " << iso << endl;

    return 0;
}
```

Output:
```
Dagens dato er: Wed Jan 20 15:07:03 2021
Dagens dato i ISO-format er: 2021-01-20
```

# Dypdykk:

Konvertering av datoer til strenger har vært en nødvendig oppgave siden begynnelsen av dataprogrammering. Før datamaskiner og digitale formater var vanlige, var det vanlig å representere datoer i tekstformat. Det er fortsatt praktisk å konvertere datoer til strenger for å lagre eller vise dem i dagens dataprogrammer. Det finnes også alternative måter å arbeide med datoer på, som å bruke numeriske formater som Unix-tidsstempler eller å bruke spesifikke klasser som `std::chrono` for å håndtere datoer.

Implementeringen av å konvertere datoer til strenger kan variere avhengig av programmeringsspråk og verktøy som brukes. I eksempelet ovenfor bruker vi `ctime` og `strftime` funksjoner fra C biblioteket. Det finnes også flere biblioteker som tilbyr mer avanserte funksjoner og formater for å konvertere datoer til strenger.

# Se Også:

- [std::chrono biblioteket](https://en.cppreference.com/w/cpp/chrono)
- [Formatere datoer med std::put_time](https://en.cppreference.com/w/cpp/io/manip/put_time)