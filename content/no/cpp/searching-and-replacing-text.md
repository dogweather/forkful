---
title:                "Søking og erstatting av tekst"
html_title:           "C++: Søking og erstatting av tekst"
simple_title:         "Søking og erstatting av tekst"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/cpp/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Søk og erstatt tekst er en vanlig teknikk brukt av programmerere for å bytte ut forekommende deler av en tekst med en annen del. Dette kan være nyttig for å endre variabler, fikse feil eller gjøre større endringer i koden. Det er en rask og effektiv måte å gjøre flere endringer i en tekst på en gang.

## Slik gjør du det:

For å søke og erstatte tekst i C++, kan du bruke funksjoner som `find()` og `replace()`. Her er et eksempel på en funksjon som bytter ut alle forekomster av ordet "hallo" i en tekst med ordet "hei":

```C++
#include <iostream>
#include <string>

using namespace std;

int main() {
    string tekst = "Hallo verden! Hallo alle sammen!";
    tekst.replace(tekst.find("hallo"), 5, "hei");

    cout << tekst << endl;
    return 0;
}
```

**Output:**
```Hei verden! Hei alle sammen!```

## Dypdykk:

Søking og erstatting av tekst har vært en del av programmering siden de tidlige dager av datamaskiner. Tidligere måtte man bruke kompliserte kommandoer for å gjøre dette, men i dag har de fleste programmeringsspråk innebygde funksjoner som forenkler prosessen.

Det finnes også andre måter å søke og erstatte tekst på, som for eksempel regulære uttrykk og tekstbehandlingsprogrammer. Det er viktig å være nøye med søkeord og erstatningsord for å unngå utilsiktede endringer i koden.

## Se også:

For mer informasjon om søking og erstatting av tekst, sjekk ut disse kildene:

- [C++ string class documentation](https://docs.microsoft.com/en-us/cpp/standard-library/basic-string-class?view=vs-2019)
- [Regular expressions in C++](https://www.geeksforgeeks.org/regular-expressions-c/)
- [Text editors for easy searching and replacing](https://www.makeuseof.com/tag/text-editors-search-replace/)