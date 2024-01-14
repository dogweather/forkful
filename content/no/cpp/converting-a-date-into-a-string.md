---
title:                "C++: Konvertering av en dato til en streng"
programming_language: "C++"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/cpp/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Hvorfor

Å konvertere en dato til en streng er en viktig del av å programmere i C++. Det kan være nyttig for å vise datoer i et brukervennlig format, som for eksempel i en kalenderapplikasjon eller en logg. Det kan også være nødvendig å lagre datoer i en streng for å lagre dem i en database eller sende dem over internett.

## Hvordan

Det er flere måter å konvertere en dato til en streng i C++. En enkel måte er å bruke funksjonen `to_string()`. La oss se på et eksempel:

```C++
#include <iostream>
#include <string>
#include <sstream>
#include <ctime>

using namespace std;

int main()
{
    // Opprett en tid som skal konverteres
    time_t now = time(0);

    // Bruk localtime() for å få den lokale tidssonen
    tm *ltm = localtime(&now);

    // Bruk stringstream for å konvertere datoen til en streng
    ostringstream oss;
    oss << ltm->tm_mday << "/" << 1 + ltm->tm_mon << "/" << 1900 + ltm->tm_year;

    // Bruk to_string() for å konvertere strengen til en heltall-string
    string date = to_string(oss.str());

    // Skriv ut datoen
    cout << date << endl;

    return 0;
}
```
**Output:**
`29/11/2021`

I dette eksempelet bruker vi funksjonen `localtime()` for å få den lokale tiden og deretter `ostringstream` for å konvertere datoen til en streng. Til slutt bruker vi `to_string()` for å konvertere strengen til en heltallsstreng, som kan være nyttig i visse situasjoner.

## Dypdykk

I tillegg til å bruke `to_string()` kan du også konvertere en dato til en streng ved hjelp av biblioteket Boost. Boost er et populært C++ bibliotek som inneholder mange nyttige funksjoner og verktøy. Det inneholder også `boost::lexical_cast` funksjonen, som kan brukes til å konvertere datoen til en streng med bare én linje kode.

La oss se på et eksempel:

```C++
#include <iostream>
#include <string>
#include <boost/lexical_cast.hpp>
#include <ctime>

using namespace std;

int main()
{
    // Opprett en tid som skal konverteres
    time_t now = time(0);

    // Bruk localtime() for å få den lokale tidssonen
    tm *ltm = localtime(&now);

    // Bruk boost::lexical_cast til å konvertere datoen til en streng
    string date = boost::lexical_cast<string>(ltm->tm_mday) + "/" + boost::lexical_cast<string>(1 + ltm->tm_mon) + "/" + boost::lexical_cast<string>(1900 + ltm->tm_year);

    // Skriv ut datoen
    cout << date << endl;

    return 0;
}
```

**Output:**
`29/11/2021`

Som du kan se, bruker vi `boost::lexical_cast` for å konvertere hvert element av datoen til en streng og deretter kombinerer dem sammen til en hel streng.

## Se Også
- [String conversion (C++) - GeeksforGeeks](https://www.geeksforgeeks.org/string-conversion-in-c/)
- [Boost C++ libraries](https://www.boost.org/)